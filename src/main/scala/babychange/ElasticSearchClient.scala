package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import babychange.ElasticSearchClient._
import babychange.filters.{CategoryFilter, FacilityFilter}
import babychange.model._
import jsonformats.ElasticSearchJsonFormats._
import spray.json._

import scala.concurrent.Future

object ElasticSearchClient {
  case class EsPlaceResponse(hits: EsPlaceHits)
  case class EsPlaceHits(hits: Vector[EsPlaceHit])
  case class EsPlaceHit(place: Place, distanceInMetres: Int)

  case class EsReviewResponse(hits: EsReviewHits, aggregations: EsAggregations)
  case class EsReviewHits(hits: Vector[EsReviewHit])
  case class EsReviewHit(_source: Review)
  case class EsAggregations(averageRating: EsAggregation)
  case class EsAggregation(value: Float)

  case class IndexResult(_id: String)
}

class ElasticSearchClient(implicit system: ActorSystem) {

  implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val executionContext = system.dispatcher

  def findPlacesNearby(lat: Double, lon: Double, facilities: FacilityFilter, categories: CategoryFilter): Future[PlaceSearchResults] = {
    val request = HttpRequest(
      uri = "http://localhost:9200/places/place/_search",
      entity = HttpEntity(
        s"""
             {
               "query": {
                 "filtered": {
                   "filter": {
                     "bool": {
                       "must": [
                         ${ if(categories.names.nonEmpty) categories.toElasticSearchQuery + "," else "" }
                         ${ if(facilities.facilities.nonEmpty) facilities.toElasticSearchQueries + "," else "" }
                         {
                           "geo_distance": {
                             "distance": "50km",
                             "location": {
                               "lat":  $lat,
                               "lon": $lon
                             }
                           }
                         }
                       ]
                     }
                   }
                 }
               },
               "sort": [
                   {
                     "_geo_distance": {
                       "location": {
                         "lat": $lat,
                         "lon": $lon
                       },
                       "order": "asc",
                       "unit": "m",
                       "distance_type": "plane"
                     }
                   }
                 ]
             }
           """.parseJson.compactPrint)
    )

//    println(request)

    val response = Http().singleRequest(request)

    for {
      r <- response
      esResponse <- Unmarshal(r.entity).to[EsPlaceResponse]
      places = esResponse.hits.hits.map { hit =>
        PlaceSearchResult(hit.place, hit.distanceInMetres)
      }
    } yield PlaceSearchResults(places)
  }

  def createReview(newReview: NewReview): Future[Review] = {
    val review = Review(newReview.rating, UtcDate.now, newReview.place, GeoLocation(0, 0), newReview.facilities, newReview.review, "-1")
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = "http://localhost:9200/reviews/review",
      entity = HttpEntity(review.toJson.compactPrint) //TODO: Prevent query injection!
    )

    Http().singleRequest(request).map { response =>
        if(response.status.isSuccess())
          review
        else
          throw new Exception("Failed to create review. Response from elastic search: " + response)
    }
  }

  def createPlace(newPlace: NewPlace): Future[Place] = {
    val facilitiesMap = newPlace.facilities.map(f => f.queryName -> f.values).toMap
    println(facilitiesMap)
    val place = Place(None, newPlace.name, newPlace.categories, newPlace.address, newPlace.phone, newPlace.location, Facilities(facilitiesMap), newPlace.openingHours)

    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = "http://localhost:9200/places/place",
      entity = HttpEntity(place.toJson.compactPrint) //TODO: Prevent query injection!
    )

    val response = Http().singleRequest(request)

    import scala.concurrent.duration._
    response.flatMap(_.entity.toStrict(5.seconds)).foreach { response =>
      println(response.toString())
    }

    response.failed.foreach(_.printStackTrace())

    for {
      r <- response
      indexResult <- Unmarshal(r.entity).to[IndexResult]
    } yield {
      place.copy(id = Some(indexResult._id))
    }
  }

  def findReviewsForPlace(placeId: String): Future[ReviewResults] = {
    //TODO: Prevent query injection
    val request = HttpRequest(
      uri = "http://localhost:9200/reviews/review/_search",
      entity = HttpEntity(
        s"""{
              "query": {
                "filtered": {
                  "filter": {
                    "term": {
                      "place": "$placeId"
                    }
                  }
                }
              },
              "sort": { "date": { "order": "desc" }},
              "aggs" : {
                "averageRating" : { "avg" : { "field" : "rating" } }
              }
            }""")
    )

    val response = Http().singleRequest(request)

    for {
      r <- response
      esResponse <- Unmarshal(r.entity).to[EsReviewResponse]
      reviews = esResponse.hits.hits.map(_._source)
    } yield ReviewResults(reviews, esResponse.aggregations.averageRating.value)
  }
}

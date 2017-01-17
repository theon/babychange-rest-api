package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import babychange.filters.{CategoryFilter, FacilityFilter}
import babychange.model._
import spray.json._

import scala.concurrent.Future

class ElasticSearchClient(implicit system: ActorSystem) {

  implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val executionContext = system.dispatcher

  case class EsPlaceResponse(hits: EsPlaceHits)
  case class EsPlaceHits(hits: Vector[EsPlaceHit])
  case class EsPlaceHit(place: Place, distanceInMetres: Int)

  implicit object EsPlaceHitFormat extends JsonFormat[EsPlaceHit] {
    override def write(obj: EsPlaceHit): JsValue = ???
    override def read(json: JsValue): EsPlaceHit = {
      val hitJson = json.asJsObject
      val placeId = hitJson.fields("_id")
      val JsArray(sorts) = hitJson.fields("sort")
      val JsNumber(distance) = sorts.head
      val placeJson = hitJson.fields("_source").asJsObject
      val place = JsObject(placeJson.fields + ("id" -> placeId)).convertTo[Place]
      EsPlaceHit(place, distance.toInt)
    }
  }
  implicit val EsPlaceHitsFormat = jsonFormat1(EsPlaceHits.apply)
  implicit val EsPlaceResponseFormat = jsonFormat1(EsPlaceResponse.apply)

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

  def createReview(newReview: NewReview): Future[NewReviewResponse] = {
    val review = Review(newReview.rating, UtcDate.now, newReview.place, GeoLocation(0, 0), newReview.facilities, newReview.review, "-1")
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = "http://localhost:9200/reviews/review",
      entity = HttpEntity(review.toJson.compactPrint) //TODO: Prevent query injection!
    )

    Http().singleRequest(request).map(response => NewReviewResponse(response.status.isSuccess(), None))
  }

  case class EsReviewResponse(hits: EsReviewHits, aggregations: EsAggregations)
  case class EsReviewHits(hits: Vector[EsReviewHit])
  case class EsReviewHit(_source: Review)
  case class EsAggregations(averageRating: EsAggregation)
  case class EsAggregation(value: Float)

  implicit val EsAggregationFormat = jsonFormat1(EsAggregation.apply)
  implicit val EsAggregationsFormat = jsonFormat1(EsAggregations.apply)
  implicit val EsReviewHitFormat = jsonFormat1(EsReviewHit.apply)
  implicit val EsReviewHitsFormat = jsonFormat1(EsReviewHits.apply)
  implicit val EsReviewResponseFormat = jsonFormat2(EsReviewResponse.apply)

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

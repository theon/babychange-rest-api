package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import spray.json._
import babychange.model._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}

import scala.concurrent.Future

class ElasticSearchClient(implicit system: ActorSystem) {

  implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val executionContext = system.dispatcher

  case class EsResponse(hits: EsHits)
  case class EsHits(hits: Vector[EsHit])
  case class EsHit(_source: Place, sort: Vector[Double])

  implicit val EsHitFormat = jsonFormat2(EsHit.apply)
  implicit val EsHitsFormat = jsonFormat1(EsHits.apply)
  implicit val EsResponseFormat = jsonFormat1(EsResponse.apply)

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

    println(request)

    val response = Http().singleRequest(request)

    for {
      r <- response
      esResponse <- Unmarshal(r.entity).to[EsResponse]
      places = esResponse.hits.hits.map { hit =>
        val place = hit._source
        val distance = hit.sort.headOption.getOrElse(-1.0).toInt
        PlaceSearchResult(place, distance)
      }
    } yield PlaceSearchResults(places)
  }
}

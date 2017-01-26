package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import babychange.ElasticSearchClient._
import babychange.filters.{CategoryFilter, FacilityFilter}
import babychange.model._
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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

  object JsonFormats extends DefaultJsonProtocol {

    implicit object UtcDateFormat extends JsonFormat[UtcDate] {
      override def read(json: JsValue): UtcDate = json match {
        case JsString(s) => UtcDate.parse(s)
        case x => deserializationError(x + " is not a valid UtcDate")
      }
      override def write(date: UtcDate): JsValue = JsString(date.toString)
    }

    implicit object TimeOfDayFormat extends JsonFormat[TimeOfDay] {
      override def read(json: JsValue): TimeOfDay = json match {
        case JsNumber(minutesSinceMidnight) => TimeOfDay(minutesSinceMidnight.toShort)
        case x => deserializationError("TimeOfDay expected JsNumber, got " + x)
      }
      override def write(obj: TimeOfDay): JsValue = JsNumber(obj.minutesSinceMidnight)
    }
    implicit val DayOpeningHoursFormat: RootJsonFormat[DayOpeningHours] = jsonFormat2(DayOpeningHours.apply)
    implicit val OpeningHoursFormat: RootJsonFormat[OpeningHours] = jsonFormat7(OpeningHours.apply)
    implicit val GeoLocationFormat: RootJsonFormat[GeoLocation] = jsonFormat2(GeoLocation.apply)

    implicit val FacilityTagFormat: RootJsonFormat[FacilityTag] = jsonFormat2(FacilityTag.apply)
    implicit val FacilityFormat: RootJsonFormat[Facility] = new RootJsonFormat[Facility] {
      override def write(obj: Facility): JsValue = {
        JsObject()
      }

      override def read(json: JsValue): Facility = ???
    }

    implicit val EsPlaceFormat: RootJsonFormat[Place] = new RootJsonFormat[Place] {
      val delegate = jsonFormat8(Place.apply)
      override def write(place: Place): JsValue = {
        val facilityTags: Map[String,Vector[String]] =
          place.facilities.groupBy(_.queryName).mapValues(_.flatMap(_.tagQueryNames))
        JsObject(
          "name" -> JsString(place.name),
          "address" -> JsString(place.address),
          "phone" -> JsString(place.phone),
          "categories" -> JsString(place.categories),
          "location" -> place.location.toJson,
          "openingHours" -> place.openingHours.toJson,
          "facilities" -> facilityTags.toJson
        )
      }
      override def read(json: JsValue) =
        deserializationError("Use EsPlaceHitFormat, rather than EsPlaceFormat directly!")
    }

    implicit val ReviewFormat: RootJsonFormat[Review] = jsonFormat7(Review.apply)

    implicit object EsPlaceHitFormat extends JsonFormat[EsPlaceHit] {
      override def write(obj: EsPlaceHit): JsValue = ???
      override def read(json: JsValue): EsPlaceHit = {
        val hitJson = json.asJsObject
        val JsString(placeId) = hitJson.fields("_id")
        val JsArray(sorts) = hitJson.fields("sort")
        val JsNumber(distance) = sorts.head
        val placeJsonNoId = hitJson.fields("_source").asJsObject
        val place  = readPlace(placeId, placeJsonNoId)
        EsPlaceHit(place, distance.toInt)
      }

      protected def readPlace(id: String, json: JsObject): Place = {
        json.getFields("name", "address", "phone", "categories", "location", "facilities") match {
          case Seq(JsString(name), JsString(address), JsString(phone), JsString(categories), location, JsObject(facilitiesFields)) =>
            val locationObj = location.convertTo[GeoLocation]
            val openingHoursObj = json.fields.get("openingHours").filterNot(_ == JsNull).map(_.convertTo[OpeningHours])
            val facilities = facilitiesFields.map { case (facilityQueryName, tagsJson) =>
              val tagsVector = tagsJson match {
                case JsArray(tags) => tags.collect { case JsString(s) => FacilityTag(s) }
                case JsString(tag) => Vector(FacilityTag(tag))
                case tags => deserializationError(s"Expected JsString or JsArray for FacilityTag(s), but for $facilityQueryName, got $tags")
              }
              Facility(facilityQueryName, tagsVector)
            }
            Place(id, name, categories, address, phone, locationObj, facilities.toVector, openingHoursObj)
        }
      }
    }
    implicit val EsPlaceHitsFormat: RootJsonFormat[EsPlaceHits] = jsonFormat1(EsPlaceHits.apply)
    implicit val EsPlaceResponseFormat: RootJsonFormat[EsPlaceResponse] = jsonFormat1(EsPlaceResponse.apply)

    implicit val EsAggregationFormat: RootJsonFormat[EsAggregation] = jsonFormat1(EsAggregation.apply)
    implicit val EsAggregationsFormat: RootJsonFormat[EsAggregations] = jsonFormat1(EsAggregations.apply)
    implicit val EsReviewHitFormat: RootJsonFormat[EsReviewHit] = jsonFormat1(EsReviewHit.apply)
    implicit val EsReviewHitsFormat: RootJsonFormat[EsReviewHits] = jsonFormat1(EsReviewHits.apply)
    implicit val EsReviewResponseFormat: RootJsonFormat[EsReviewResponse] = jsonFormat2(EsReviewResponse.apply)

    implicit val IndexResultFormat: RootJsonFormat[IndexResult] = jsonFormat1(IndexResult.apply)
  }
}

class ElasticSearchClient(implicit system: ActorSystem) {

  import ElasticSearchClient.JsonFormats._

  implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val executionContext: ExecutionContext = system.dispatcher

  def config = system.settings.config

  // TODO: AWS Signing: http://169.254.169.254/latest/meta-data/iam/security-credentials/babychange-prod-restapi-RestApiBackendRole-1BL927QD80RBM

  val signer: SignatureV4Signer = Try(config.getString("babychange.elasticsearch.iamRole")) match {
    case Success(role) => println("ec2Instance signer!"); SignatureV4Signer.ec2Instance(role)
    case Failure(_) => println("environmentVariables signer!"); SignatureV4Signer.environmentVariables()
  }

  val baseUrl: String = config.getString("babychange.elasticsearch.baseUrl")

  def findPlacesNearby(lat: Double, lon: Double, facilities: FacilityFilter, categories: CategoryFilter): Future[PlaceSearchResults] = {
    val request = HttpRequest(
      uri = s"$baseUrl/places/place/_search",
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

    val response = singleRequest(request)

    for {
      r <- response
      esResponse <- Unmarshal(r.entity).to[EsPlaceResponse]
      places = esResponse.hits.hits.map { hit =>
        PlaceSearchResult(hit.place, hit.distanceInMetres)
      }
    } yield PlaceSearchResults(places)
  }

  def createReview(user: String, newReview: NewReview): Future[Review] = {
    val review = Review(newReview.rating, UtcDate.now, newReview.place, GeoLocation(0, 0), newReview.facilities, newReview.review, user)
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$baseUrl/reviews/review",
      entity = HttpEntity(review.toJson.compactPrint) //TODO: Prevent query injection!
    )

    singleRequest(request).map { response =>
        if(response.status.isSuccess())
          review
        else
          throw new Exception("Failed to create review. Response from elastic search: " + response)
    }
  }

  def createPlace(place: Place): Future[Place] = {

    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$baseUrl/places/place",
      entity = HttpEntity(place.toJson.compactPrint) //TODO: Prevent query injection!
    )

    val response = singleRequest(request)

    import scala.concurrent.duration._
    response.flatMap(_.entity.toStrict(5.seconds)).foreach { response =>
      println(response.toString())
    }

    response.failed.foreach(_.printStackTrace())

    for {
      r <- response
      indexResult <- Unmarshal(r.entity).to[IndexResult]
    } yield {
      place.copy(id = indexResult._id)
    }
  }

  def findReviewsForPlace(placeId: String): Future[ReviewResults] = {
    //TODO: Prevent query injection
    val request = HttpRequest(
      uri = s"$baseUrl/reviews/review/_search",
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

    val response = singleRequest(request)

    for {
      r <- response
      esResponse <- Unmarshal(r.entity).to[EsReviewResponse]
      reviews = esResponse.hits.hits.map(_._source)
    } yield ReviewResults(reviews, esResponse.aggregations.averageRating.value)
  }

//  val connectionFlow: Flow[HttpRequest, HttpResponse, Future[Http.OutgoingConnection]] =
//    Http().outgoingConnection("search-babychange-prod-xdgcfgb2jc4tlmf7s6b4z3g62m.eu-west-1.es.amazonaws.com", 443)
  
  def singleRequest(request: HttpRequest): Future[HttpResponse] = {
    //Source.single(request).via(signer.sign).via(connectionFlow).runWith(Sink.head)
    val res = Http().singleRequest(signer.sign(request))

    res.foreach { response =>
      response.entity.dataBytes.runForeach(bs => println(bs.decodeString("UTF-8")))

    }

    res
  }
}

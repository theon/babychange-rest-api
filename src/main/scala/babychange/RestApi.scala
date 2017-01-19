package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RejectionError, ValidationRejection}
import akka.stream.ActorMaterializer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes
import babychange.filters._
import babychange.model._
import jsonformats.PublicApiJsonFormats._
import spray.json._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

trait RestApi {

  implicit val system = ActorSystem("babychange-system")
  implicit val materializer = ActorMaterializer()

  val elasticSearchClient = new ElasticSearchClient

  implicit def ec: ExecutionContext = system.dispatcher

  val Latitude = DoubleNumber.flatMap { lat =>
    if(lat < -90 || lat > 90)
      throw new RejectionError(ValidationRejection("Latitude must be between -90 and 90"))
    else
      Some(lat)
  }

  val Longitude = DoubleNumber.flatMap { lon =>
    if(lon < -180 || lon > 180) None else Some(lon)
  }

  val facilityFilters =  extractRequest map { r =>
    val facilityFiltersTuples = r.uri.query().getAll("facility").map(_ split ':').collect {
      case Array(name, value) => name -> value
    }
    FacilityFilter(facilityFiltersTuples.toVector)
  }

  val categoryFilters = parameter("categories".?) map { p =>
    CategoryFilter(p.map(_.split(',').toVector).getOrElse(Vector.empty))
  }

  val filters = facilityFilters & categoryFilters

  val route =
    get {
      path("places" / Latitude ~ "," ~ Longitude) { (lat, lon) =>
        filters { (facilities, categories) =>
          complete(elasticSearchClient.findPlacesNearby(lat, lon, facilities, categories))
        }
      } ~
      path("reviews") {
        parameter("place") { placeId =>
          complete(elasticSearchClient.findReviewsForPlace(placeId).map(_.toJson))
        }
      } ~
      path("filters") {
        complete(allowedFilters)
      }
    } ~
    post {
      path("places") {
        entity(as[NewPlace]) { newPlace: NewPlace =>
          onComplete(elasticSearchClient.createPlace(newPlace)) {
            case Success(place) =>
              complete(place)
            case Failure(e) =>
              complete(500, "Failed to create place")
          }
        }
      } ~
      path("reviews") {
        entity(as[NewReview]) { newReview: NewReview =>
                //TODO: Auth
          onComplete(elasticSearchClient.createReview(newReview)) {
            case Success(review) =>
                complete(review.toJson)
            case Failure(e) =>
              complete(500, "Failed to create review")
          }
        }
      }
    }
}

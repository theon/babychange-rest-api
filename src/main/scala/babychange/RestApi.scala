package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, RejectionError, ValidationRejection}
import akka.stream.ActorMaterializer
import babychange.filters._
import babychange.model.JsonFormats._
import babychange.model._

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

  val crappyAuth: Directive1[String] = headerValueByName("Authorization").flatMap {
    case "Ian" => provide("Ian")
    case "Frances" => provide("Frances")
    case _ => reject
  }

  val route = crappyAuth { user =>
    get {
      path("places" / Latitude ~ "," ~ Longitude) { (lat, lon) =>
        filters { (facilities, categories) =>
          complete(elasticSearchClient.findPlacesNearby(lat, lon, facilities, categories))
        }
      } ~
      path("reviews") {
        parameter("place") { placeId =>
          complete(elasticSearchClient.findReviewsForPlace(placeId))
        }
      } ~
      path("filters") {
        complete(AllowedFilters(AllFacilities.allFacilities))
      }
    } ~
    post {
      path("places") {
        entity(as[Place]) { newPlace: Place =>
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
          onComplete(elasticSearchClient.createReview(user, newReview)) {
            case Success(review) =>
                complete(review)
            case Failure(e) =>
              complete(500, "Failed to create review")
          }
        }
      }
    }
  }
}


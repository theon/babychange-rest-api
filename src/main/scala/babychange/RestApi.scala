package babychange

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RejectionError, ValidationRejection}
import akka.stream.ActorMaterializer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import babychange.model._

trait RestApi {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()

  val elasticSearchClient = new ElasticSearchClient

  val Latitude = DoubleNumber.flatMap { lat =>
    if(lat < -90 || lat > 90)
      throw new RejectionError(ValidationRejection("Latitude must be between -90 and 90"))
    else
      Some(lat)
  }

  val Longitude = DoubleNumber.flatMap { lon =>
    if(lon < -180 || lon > 180) None else Some(lon)
  }

  val facilityFilters = parameter("facilities".?) map { p =>
    val facilityFiltersStrs = p.map(_.split(',').toVector).getOrElse(Vector.empty)
    val facilityFiltersTuples = facilityFiltersStrs map { facilityFilter =>
      val Array(name, value) = facilityFilter.split(':')
      name -> value
    }
    FacilityFilter(facilityFiltersTuples)
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
      }
    }
}

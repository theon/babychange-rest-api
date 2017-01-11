package babychange

import spray.json.{JsonFormat, _}

object model extends DefaultJsonProtocol {

  case class TimeOfDay(minutesSinceMidnight: Short) {
    override def toString: String = {
      val hours = minutesSinceMidnight / 60
      val minutes = (minutesSinceMidnight % 60).toString match {
        case s if s.length == 1 => "0" + s
        case s => s
      }
      s"$hours:$minutes"
    }
  }

  case class DayOpeningHours(open: TimeOfDay, close: TimeOfDay)

  case class OpeningHours(
     mon: DayOpeningHours,
     tue: DayOpeningHours,
     wed: DayOpeningHours,
     thu: DayOpeningHours,
     fri: DayOpeningHours,
     sat: DayOpeningHours,
     sun: DayOpeningHours
   )

  case class GeoLocation(lat: BigDecimal, lon: BigDecimal)

  case class Place(name: String, categories: String, address: String, phone: String, location: GeoLocation, facilities: Map[String, Vector[String]], openingHours: OpeningHours) {
    require(facilities.values.forall(tags => tags.contains("Yes") || tags.contains("No") || tags.contains("Unknown")), "All facilities must contain a Yes or No or Unknown. " + this.toString)
  }

  case class PlaceSearchResult(place: Place, distanceInMetres: Int)

  case class PlaceSearchResults(places: Vector[PlaceSearchResult])

  //Filters

  case class FacilityFilter(facilities: Vector[(String,String)]) {
    //TODO Prevent query injection
    def toElasticSearchQueries = facilities.map { case (name, value) => s"""{ "term":  { "facilities.$name": "$value" }}""" }.mkString(",")
  }

  case class CategoryFilter(names: Vector[String]) {
    //TODO Prevent query injection
    def toElasticSearchQuery = s"""{ "terms":  { "categories": [ ${names.map("\"" + _ + "\"").mkString(",")} ] }}"""
  }

  // JSON Formats

//  override implicit def vectorFormat[T :JsonFormat] = new JsonFormat[Vector[T]] {
//
//  }

  implicit object EsVectorFormat extends JsonFormat[Vector[String]] {
    override def read(json: JsValue): Vector[String] = json match {
      case JsString(s) => Vector(s)
      case JsArray(elements) => elements collect { case JsString(s) => s }
      case x => deserializationError(x + " is not a valid Vector[String]")
    }
    override def write(strings: Vector[String]): JsValue = JsArray(strings.map(JsString.apply))
  }

  implicit object TimeOfDayFormat extends JsonFormat[TimeOfDay] {
    override def read(json: JsValue): TimeOfDay = json match {
      case JsNumber(num) => TimeOfDay(num.toShort)
      case x => deserializationError(x + " is not a valid TimeOfDay")
    }
    override def write(obj: TimeOfDay): JsValue = JsString(obj.toString)
  }

  implicit val DayOpeningHoursFormat = jsonFormat2(DayOpeningHours.apply)
  implicit val OpeningHoursFormat = jsonFormat7(OpeningHours.apply)
  implicit val GeoLocationFormat = jsonFormat2(GeoLocation.apply)
  implicit val PlaceFormat = jsonFormat7(Place.apply)
  implicit val PlaceSearchResultFormat = jsonFormat2(PlaceSearchResult.apply)
  implicit val PlaceSearchResultsFormat = jsonFormat1(PlaceSearchResults.apply)
}

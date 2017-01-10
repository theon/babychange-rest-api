package babychange

import spray.json.DefaultJsonProtocol._
import spray.json._

object model {

  sealed trait Availability
  case object Yes extends Availability
  case object YesVerified extends Availability
  case object No extends Availability
  case object NoVerified extends Availability
  case object Unknown extends Availability

  case class Facilities(
     babyChanging: Availability,
     highchairs: Availability,
     highchairTrays: Availability,
     kidsMenu: Availability
  )

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

  case class Place(name: String, address: String, phone: String, location: GeoLocation, facilities: Facilities, openingHours: OpeningHours)

  case class PlaceSearchResult(place: Place, distanceInMetres: Int)

  case class PlaceSearchResults(places: Vector[PlaceSearchResult])


  implicit object AvailabilityFormat extends JsonFormat[Availability] {
    override def read(json: JsValue): Availability = json match {
      case JsString("Yes") => Yes
      case JsString("YesVerified") => YesVerified
      case JsString("No") => No
      case JsString("NoVerified") => NoVerified
      case JsString("Unknown") => Unknown
      case x => deserializationError(x + " is not a valid Availability")
    }
    override def write(obj: Availability): JsValue = JsString(obj.toString)
  }

  implicit val FacilitiesFormat = jsonFormat4(Facilities.apply)

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
  implicit val PlaceFormat = jsonFormat6(Place.apply)
  implicit val PlaceSearchResultFormat = jsonFormat2(PlaceSearchResult.apply)
  implicit val PlaceSearchResultsFormat = jsonFormat1(PlaceSearchResults.apply)
}

package babychange

import akka.http.scaladsl.model.DateTime
import spray.json.{JsonFormat, _}

object model extends DefaultJsonProtocol {

  case class  TimeOfDay(minutesSinceMidnight: Short) {
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

  case class Facilities(values: Map[String, Vector[String]]) {
    require(values.values.forall(tags => tags.contains("Yes") || tags.contains("No") || tags.contains("Unknown")), "All facilities must contain a Yes or No or Unknown. " + this.toString)
  }

  case class Place(id: String, name: String, categories: String, address: String, phone: String, location: GeoLocation, facilities: Facilities, openingHours: OpeningHours)

  case class PlaceSearchResult(place: Place, distanceInMetres: Int)

  case class PlaceSearchResults(places: Vector[PlaceSearchResult])

  object UtcDate {
    def now = {
      val DateTime(year, month, day, _, _, _, _, _, _) = DateTime.now
      UtcDate(year, month, day)
    }
    def parse(s: String) = s.split('-') match {
      case Array(year, month, day) =>
        UtcDate(year.toInt, month.toInt, day.toInt)
      case _ => throw new IllegalArgumentException(s + " is not a valid date")
    }
  }
  case class UtcDate(year: Int, month: Int, day: Int) {
    require(year > 1999 && year < 2101, "Year should be between 2000 and 2100")
    require(month > 0 && month < 13, "Month should be between 1 and 12")
    require(day > 0 && day < 32, "Day should be between 1 and 31")

    override def toString() = s"$year-$month-$day"
  }

  case class NewReview(rating: Float, place: String, facilities: Vector[String], review: String) {
    require(rating >= 0.0 && rating <= 5.0, "rating must be between 0.0 and 5.0")
  }
  case class NewReviewResponse(success: Boolean, cause: Option[String])
  case class Review(rating: Float, date: UtcDate, place: String, placeLocation: GeoLocation, facilities: Vector[String], review: String, user: String)
  case class ReviewResults(reviews: Vector[Review], averageRating: Float)

  // JSON Formats

//  override implicit def vectorFormat[T :JsonFormat] = new JsonFormat[Vector[T]] {
//
//  }

  implicit object UtcDateFormat extends JsonFormat[UtcDate] {
    override def read(json: JsValue): UtcDate = json match {
      case JsString(s) => UtcDate.parse(s)
      case x => deserializationError(x + " is not a valid UtcDate")
    }
    override def write(date: UtcDate): JsValue = JsString(date.toString)
  }

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

  implicit object FacilitiesFormat extends JsonFormat[Facilities] {
    override def write(obj: Facilities): JsValue = {
      val facilitiesJsons = obj.values.map { case (name, values) =>
        JsObject("name" -> JsString(filters.facilityFilterNameLookup.getOrElse(name, name)), "queryName" -> JsString(name), "values" -> values.toJson)
      }
      JsArray(facilitiesJsons.toVector)
    }

    override def read(json: JsValue): Facilities =
      Facilities(json.convertTo[Map[String,Vector[String]]])
  }

  implicit val DayOpeningHoursFormat = jsonFormat2(DayOpeningHours.apply)
  implicit val OpeningHoursFormat = jsonFormat7(OpeningHours.apply)
  implicit val GeoLocationFormat = jsonFormat2(GeoLocation.apply)
  implicit val PlaceFormat = jsonFormat8(Place.apply)
  implicit val PlaceSearchResultFormat = jsonFormat2(PlaceSearchResult.apply)
  implicit val PlaceSearchResultsFormat = jsonFormat1(PlaceSearchResults.apply)

  implicit val NewReviewFormat = jsonFormat4(NewReview.apply)
  implicit val NewReviewResponseFormat = jsonFormat2(NewReviewResponse.apply)
  implicit val ReviewFormat = jsonFormat7(Review.apply)
  implicit val ReviewResultsFormat = jsonFormat2(ReviewResults.apply)
}

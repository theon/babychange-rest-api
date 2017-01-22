package babychange

import akka.http.scaladsl.model.DateTime
import com.typesafe.config.{Config, ConfigFactory}
import spray.json.DefaultJsonProtocol.jsonFormat1
import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsString, JsValue, JsonFormat, JsonReader, RootJsonFormat, deserializationError}

object model {

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

  case class AllowedFilters(facility: Vector[Facility])

  object Facility {
    def apply(queryName: String, tags: Vector[FacilityTag]): Facility = {
      val displayName = AllFacilities.facilityQueryToDisplayName.getOrElse(queryName, queryName)
      new Facility(displayName, queryName, tags)
    }
  }
  case class Facility(displayName: String, queryName: String, tags: Vector[FacilityTag]) {
    require(isYes || isNo || isUnknown, s"Facility $displayName must contain a Yes or No or Unknown, but only got $tags")
    def isYes: Boolean = tagExists(FacilityTag.Yes)
    def isNo: Boolean = tagExists(FacilityTag.No)
    def isUnknown: Boolean = tagExists(FacilityTag.Unknown)
    def tagExists(tag: String): Boolean = tags.exists(_.queryName == tag)
    def tagQueryNames: Vector[String] = tags.map(_.queryName)
  }
  object FacilityTag {
    val Yes = "yes"
    val No = "no"
    val Unknown = "unknown"

    def apply(queryName: String): FacilityTag = {
      val displayName = AllFacilities.facilityTagQueryToDisplayName.getOrElse(queryName, queryName)
      new FacilityTag(displayName, queryName)
    }
  }
  case class FacilityTag(displayName: String, queryName: String)

  object Place {
    val noId: (String, String, String, String, GeoLocation, Vector[Facility], Option[OpeningHours]) => Place =
      Place.apply("NOID", _, _, _, _, _, _, _)
  }

  case class Place(id: String, name: String, categories: String, address: String, phone: String, location: GeoLocation, facilities: Vector[Facility], openingHours: Option[OpeningHours])

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
  case class Review(rating: Float, date: UtcDate, place: String, placeLocation: GeoLocation, facilities: Vector[String], review: String, user: String)
  case class ReviewResults(reviews: Vector[Review], averageRating: Float)

  object JsonFormats extends DefaultJsonProtocol {

    override protected def fromField[T](value: JsValue, fieldName: String)
                              (implicit reader: JsonReader[T]) = value match {
      case x: JsObject if
      (reader.isInstanceOf[OptionFormat[_]] &
              !x.fields.contains(fieldName)) =>
        None.asInstanceOf[T]
      case x: JsObject if
      (reader.isInstanceOf[StringJsonFormat.type] &
              !x.fields.contains(fieldName)) =>
        "".asInstanceOf[T]
      case x: JsObject =>
        try reader.read(x.fields(fieldName))
        catch {
          case e: NoSuchElementException =>
            deserializationError("Object is missing required member '" + fieldName + "'", e, fieldName :: Nil)
          case DeserializationException(msg, cause, fieldNames) =>
            deserializationError(msg, cause, fieldName :: fieldNames)
        }
      case _ => deserializationError("Object expected in field '" + fieldName + "'", fieldNames = fieldName :: Nil)
    }

    implicit object UtcDateFormat extends JsonFormat[UtcDate] {
      override def read(json: JsValue): UtcDate = json match {
        case JsString(s) => UtcDate.parse(s)
        case x => deserializationError(x + " is not a valid UtcDate")
      }
      override def write(date: UtcDate): JsValue = JsString(date.toString)
    }

    implicit object TimeOfDayFormat extends JsonFormat[TimeOfDay] {
      override def read(json: JsValue): TimeOfDay = ??? //TODO Implement
      override def write(obj: TimeOfDay): JsValue = JsString(obj.toString)
    }

    implicit val DayOpeningHoursFormat: RootJsonFormat[DayOpeningHours] = jsonFormat2(DayOpeningHours.apply)
    implicit val OpeningHoursFormat: RootJsonFormat[OpeningHours] = jsonFormat7(OpeningHours.apply)
    implicit val GeoLocationFormat: RootJsonFormat[GeoLocation] = jsonFormat2(GeoLocation.apply)

    implicit val FacilityTagFormat: RootJsonFormat[FacilityTag] = jsonFormat2(FacilityTag.apply)
    implicit val FacilityFormat: RootJsonFormat[Facility] = jsonFormat3(Facility.apply)
    implicit val AllowedFiltersFormat: RootJsonFormat[AllowedFilters] = jsonFormat1(AllowedFilters.apply)

    implicit val PlaceFormat: RootJsonFormat[Place] = jsonFormat8(Place.apply)

//    implicit val PlaceFormat: RootJsonFormat[Place] = new RootJsonFormat[Place] {
//      val delegate = jsonFormat8(Place.apply)
//
//      override def write(obj: Place): JsValue = delegate.write(obj)
//
//      override def read(json: JsValue) = {
//        val placeWithDummyId = JsObject(json.asJsObject.fields + ("id" -> JsString("")))
//        delegate.read(placeWithDummyId)
//      }
//    }

    implicit val PlaceSearchResultFormat: RootJsonFormat[PlaceSearchResult] = jsonFormat2(PlaceSearchResult.apply)
    implicit val PlaceSearchResultsFormat: RootJsonFormat[PlaceSearchResults] = jsonFormat1(PlaceSearchResults.apply)

    implicit val NewReviewFormat: RootJsonFormat[NewReview] = jsonFormat4(NewReview.apply)
    implicit val ReviewFormat: RootJsonFormat[Review] = jsonFormat7(Review.apply)
    implicit val ReviewResultsFormat: RootJsonFormat[ReviewResults] = jsonFormat2(ReviewResults.apply)
  }
}

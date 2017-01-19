package babychange.jsonformats

import babychange.filters
import babychange.model._
import spray.json._

trait CommonJsonFormats extends DefaultJsonProtocol {
  implicit object UtcDateFormat extends JsonFormat[UtcDate] {
    override def read(json: JsValue): UtcDate = json match {
      case JsString(s) => UtcDate.parse(s)
      case x => deserializationError(x + " is not a valid UtcDate")
    }
    override def write(date: UtcDate): JsValue = JsString(date.toString)
  }

  object EsVectorFormat extends JsonFormat[Vector[String]] {
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

  implicit val FacilityFormat = jsonFormat3(Facility.apply)

  implicit val ReviewFormat: JsonFormat[Review] = jsonFormat7(Review.apply)
}
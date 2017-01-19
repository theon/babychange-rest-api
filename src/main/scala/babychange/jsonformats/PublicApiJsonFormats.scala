package babychange.jsonformats

import babychange.filters
import babychange.model._
import spray.json.{JsArray, JsObject, JsString, JsValue, JsonFormat}

trait PublicApiJsonFormats extends CommonJsonFormats {

  implicit object FacilitiesFormat extends JsonFormat[Facilities] {
    override def write(obj: Facilities): JsValue = {
      val facilitiesJsons = obj.values.map { case (name, values) =>
        JsObject("name" -> JsString(filters.facilityFilterNameLookup.getOrElse(name, name)), "queryName" -> JsString(name), "values" -> JsArray(values.map(JsString.apply)))
      }
      JsArray(facilitiesJsons.toVector)
    }

    override def read(json: JsValue): Facilities =
      ???
  }

  implicit val PlaceFormat = jsonFormat8(Place.apply)

  implicit val PlaceSearchResultFormat = jsonFormat2(PlaceSearchResult.apply)
  implicit val PlaceSearchResultsFormat = jsonFormat1(PlaceSearchResults.apply)

  implicit val NewPlaceFormat = jsonFormat7(NewPlace.apply)

  implicit val NewReviewFormat = jsonFormat4(NewReview.apply)
  implicit val ReviewResultsFormat: JsonFormat[ReviewResults] = jsonFormat2(ReviewResults.apply)
}

object PublicApiJsonFormats extends PublicApiJsonFormats
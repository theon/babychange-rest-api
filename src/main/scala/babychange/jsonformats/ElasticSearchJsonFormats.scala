package babychange.jsonformats

import babychange.ElasticSearchClient._
import babychange.filters
import babychange.model.{Facilities, NewPlace, Place}
import spray.json.{JsArray, JsString, _}

trait ElasticSearchJsonFormats extends CommonJsonFormats {

  implicit object FacilitiesFormat extends JsonFormat[Facilities] {
    override def write(obj: Facilities): JsValue =
      obj.values.toJson

    override def read(json: JsValue): Facilities = {
      val map = json.asJsObject.fields.mapValues {
        case JsArray(values) => values.map(_.convertTo[String])
        case JsString(value) => Vector(value)
        case x => deserializationError("Unexpected facility values: " + x)
      }
      Facilities(map)
    }
  }
  implicit val PlaceFormat = jsonFormat8(Place.apply)

  implicit val NewPlaceFormat = jsonFormat7(NewPlace.apply)

  implicit object EsPlaceHitFormat extends JsonFormat[EsPlaceHit] {
    override def write(obj: EsPlaceHit): JsValue = ???
    override def read(json: JsValue): EsPlaceHit = {
      val hitJson = json.asJsObject
      val placeId = hitJson.fields("_id")
      val JsArray(sorts) = hitJson.fields("sort")
      val JsNumber(distance) = sorts.head
      val placeJson = hitJson.fields("_source").asJsObject
      val place = JsObject(placeJson.fields + ("id" -> placeId)).convertTo[Place]
      EsPlaceHit(place, distance.toInt)
    }
  }
  implicit val EsPlaceHitsFormat = jsonFormat1(EsPlaceHits.apply)
  implicit val EsPlaceResponseFormat = jsonFormat1(EsPlaceResponse.apply)

  implicit val EsAggregationFormat = jsonFormat1(EsAggregation.apply)
  implicit val EsAggregationsFormat = jsonFormat1(EsAggregations.apply)
  implicit val EsReviewHitFormat = jsonFormat1(EsReviewHit.apply)
  implicit val EsReviewHitsFormat = jsonFormat1(EsReviewHits.apply)
  implicit val EsReviewResponseFormat = jsonFormat2(EsReviewResponse.apply)

  implicit val IndexResultFormat = jsonFormat1(IndexResult.apply)
}
object ElasticSearchJsonFormats extends ElasticSearchJsonFormats
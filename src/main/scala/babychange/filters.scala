package babychange

import spray.json.DefaultJsonProtocol._

object filters {

  case class AllowedFilters(facility: Vector[AllowedFilterValues])
  case class AllowedFilterValues(name: String, queryName: String, values: Vector[String])

  implicit val AllowedFilterValuesFormat = jsonFormat3(AllowedFilterValues.apply)
  implicit val AllowedFiltersFormat = jsonFormat1(AllowedFilters.apply)

  val facilityFilterNameLookup = Map(
    "babyChanging" -> "Baby Changing",
    "highchairs" -> "Highchairs",
    "kidsMenu" -> "Kids' Menu",
    "babyFeedingFriendly" -> "Baby Feeding Friendly",
    "buggyFriendly" -> "Buggy Friendly",
    "playArea" -> "Play Area"
  )

  val allowedFilters = AllowedFilters(
    facility = Vector(
      AllowedFilterValues(facilityFilterNameLookup("babyChanging"), "babyChanging", Vector("Yes", "Women's", "Men's", "Disabled", "Separate")),
      AllowedFilterValues(facilityFilterNameLookup("highchairs"), "highchairs", Vector("Yes", "With Trays")),
      AllowedFilterValues(facilityFilterNameLookup("kidsMenu"), "kidsMenu", Vector("Yes", "Baby Food")),
      AllowedFilterValues(facilityFilterNameLookup("babyFeedingFriendly"), "babyFeedingFriendly", Vector("Yes", "Private Breastfeeding")),
      AllowedFilterValues(facilityFilterNameLookup("buggyFriendly"), "buggyFriendly", Vector("Yes", "Parking", "Locks")),
      AllowedFilterValues(facilityFilterNameLookup("playArea"), "playArea", Vector("Yes", "Soft Play", "Outside"))
    )
  )

  case class FacilityFilter(facilities: Vector[(String,String)]) {
    //TODO Prevent query injection
    def toElasticSearchQueries = facilities.map { case (name, value) => s"""{ "term":  { "facilities.$name": "$value" }}""" }.mkString(",")
  }

  case class CategoryFilter(names: Vector[String]) {
    //TODO Prevent query injection
    def toElasticSearchQuery = s"""{ "terms":  { "categories": [ ${names.map("\"" + _ + "\"").mkString(",")} ] }}"""
  }

}
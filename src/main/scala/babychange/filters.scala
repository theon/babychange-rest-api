package babychange

import babychange.model.Facility

object filters {

  case class FacilityFilter(facilities: Vector[(String,String)]) {
    //TODO Prevent query injection
    def toElasticSearchQueries = facilities.map { case (name, value) => s"""{ "term":  { "facilities.$name": "$value" }}""" }.mkString(",")
  }

  case class CategoryFilter(names: Vector[String]) {
    //TODO Prevent query injection
    def toElasticSearchQuery = s"""{ "terms":  { "categories": [ ${names.map("\"" + _ + "\"").mkString(",")} ] }}"""
  }

}
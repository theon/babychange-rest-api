package babychange

import babychange.model.{Facility, FacilityTag}
import com.typesafe.config.{Config, ConfigFactory}

object AllFacilities {
  import scala.collection.JavaConverters._

  protected def tagConfigToTag(tagConfig: Config) = {
    val displayName = tagConfig.getString("displayName")
    val queryName = tagConfig.getString("queryName")
    FacilityTag(displayName, queryName)
  }

  val defaultFacilityTags: Vector[FacilityTag] = {
    val config = ConfigFactory.load()
    config.getConfigList("babychange.facilities.defaultTags").asScala.map(tagConfigToTag).toVector
  }

  val allFacilities: Vector[Facility] = {
    val config = ConfigFactory.load()
    val facilities = config.getConfigList("babychange.facilities.all").asScala.map { facilityConfig =>
      val displayName = facilityConfig.getString("displayName")
      val queryName = facilityConfig.getString("queryName")
      val extraTags = facilityConfig.getConfigList("extraTags").asScala.map(tagConfigToTag)
      Facility(displayName, queryName, defaultFacilityTags ++ extraTags)
    }
    facilities.toVector
  }

  val facilityQueryToDisplayName: Map[String, String] =
    allFacilities.map(f => f.queryName -> f.displayName).toMap

  val facilityTagQueryToDisplayName: Map[String, String] =
    allFacilities.flatMap(_.tags.map(f => f.queryName -> f.displayName)).toMap
}
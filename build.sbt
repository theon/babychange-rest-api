name := "babychange-rest-api"

organization  := "babychange"

version       := "0.0.1"

scalaVersion  := "2.12.1"

dynamoDBLocalInMemory := false
dynamoDBLocalDBPath := Some("babychange")

libraryDependencies ++=
    "com.typesafe.akka" %% "akka-http" % "10.0.1" ::
    "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.1" ::
    "io.spray" %%  "spray-json" % "1.3.2" :: Nil

// Test Dependencies
libraryDependencies ++=
  "org.scalatest" %% "scalatest" % "3.0.0" % "test" :: Nil

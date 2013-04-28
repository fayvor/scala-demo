import sbt._
import sbt.Keys._

object DemoBuild extends Build {

  lazy val project = Project(
    id = "scala-demo",
    base = file("."),
    settings = demoSettings
  )
  
  def demoSettings = Defaults.defaultSettings ++ Seq (
    scalacOptions += "-unchecked",
    scalacOptions += "-deprecation",

    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "1.14" % "test",
      "junit" % "junit" % "4.8" % "test->default"
    )

  )
}
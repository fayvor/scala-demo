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
      "junit" % "junit" % "4.8" % "test->default",
      // "com.mongodb.casbah" %% "casbah" % "2.6.0",
      "com.github.tmingos" % "casbah_2.10" % "2.5.0-SNAPSHOT",
      "com.chuusai" %% "shapeless" % "1.2.4",
      "org.scalaz" %% "scalaz-core" % "7.0.0"
    ),
    
    resolvers ++= Seq(
      "Typesafe Maven Repository" at "http://repo.typesafe.com/typesafe/maven-ivy-releases",
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases",
      "Sonatype Public Repository" at "http://oss.sonatype.org/content/groups/public/",
      "Sonatype OSS" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
      // "Novus Snapshot Repository" at "http://repo.novus.com/snapshots/",
      // "spray repo" at "http://repo.spray.cc/",
      // "Scales Repo" at "http://scala-scales.googlecode.com/svn/repo",
      // "MVN Repo" at "http://mvnrepository.com/artifact/",
      // "array.ca" at "http://www.array.ca/nest-web/maven/"
    )

  )
}
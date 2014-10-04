scalaVersion := "2.11.2"

organization := "ru.lester"

name := "robot"

version := "0.0.1-SNAPSHOT"

scalacOptions := Seq("-unchecked", "-deprecation", "-explaintypes", "-encoding", "utf8", "-feature"/*, "-Xlog-implicits"*/)

resolvers ++= Seq(
  "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

libraryDependencies ++= {
  val scalazV = "7.0.6"
  val scalazStreamV = "0.5"
  val specs2V = "2.3.11"
  val scalaLogV = "3.0.0"
  val logbackV = "1.1.2"
  Seq(
    "org.scalaz"                 %%  "scalaz-core"              % scalazV,
    "org.scalaz"                 %%  "scalaz-effect"            % scalazV,
    "org.scalaz.stream"          %%  "scalaz-stream"            % scalazStreamV,
    "org.specs2"                 %%  "specs2"                   % specs2V % "test" ,
    "com.typesafe.scala-logging" %%  "scala-logging"            % scalaLogV,
    "ch.qos.logback"             %   "logback-core"             % logbackV,
    "ch.qos.logback"             %   "logback-classic"          % logbackV
  )
}
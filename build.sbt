val scala3Version = "3.2.0"

// not work
//Compile / scalacOptions += "-target:jvm-1.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "json4s3",
    version := "2.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
    //libraryDependencies += "io.github.dataoperandz" % "cassper" % "0.3"
  )

// https://dev.to/awwsmm/publish-your-scala-project-to-maven-in-5-minutes-with-sonatype-326l

ThisBuild / organization := "xyz.cofe"
ThisBuild / organizationName := "gochaorg"
ThisBuild / organizationHomepage := Some(url("https://cofe.xyz"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/gochaorg/json4s3"),
    "scm:git@github.gochaorg/json4s3.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "gochaorg",
    name  = "Georgiy P. Kamnev",
    email = "nt.gocha@gmail.com",
    url   = url("https://cofe.xyz")
  )
)

ThisBuild / description := "json library for scala 3"
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/gochaorg/json4s3"))

ThisBuild / publishMavenStyle := true
ThisBuild / publishArtifact in Test := false

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  else
    Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2/")
}

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / sonatypeCredentialHost := "oss.sonatype.org"
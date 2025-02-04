import Dependencies._

/* Project settings */
ThisBuild / name := "pipo"
ThisBuild / scalaVersion := "2.13.15"
ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / description := "Config parser and interpolation"
ThisBuild / licenses := List(("MIT", url("https://opensource.org/license/mit")))
ThisBuild / developers ++= List(
  Developer(
    id = "csgn",
    name = "Sergen Cepoglu",
    email = "dev.csgn@gmail.com",
    url = url("https://github.com/csgn")
  )
)

/* Test settings */
ThisBuild / testFrameworks += new TestFramework("munit.Framework")

/* Scalafix settings */
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

/* Compiler settings */
ThisBuild / scalacOptions ++= Seq(
  // "-Wunused",
)

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name := "core",
    moduleName := "pipo-core",
  )
  .settings(
    libraryDependencies ++= {
      Seq(
        munit,
        cats,
        catsEffect,
      )
    }
  )
  .enablePlugins(ScalafixPlugin)

lazy val pipo = project
  .in(file("."))
  .settings(
    name := "pipo",
  )
  .aggregate(core)

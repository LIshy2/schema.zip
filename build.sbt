ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "scheme"
  )

ThisBuild / scalacOptions += "-Ykind-projector:underscores"

val catsVersion = "2.9.0"
val parsecVersion = "2.2.0"
val munitVersion = "0.7.29"
val catsMtlVersion = "1.3.0"
val scoptVersion = "4.1.0"

libraryDependencies += "com.github.scopt" %% "scopt" % scoptVersion
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-free" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-mtl" % catsMtlVersion
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % parsecVersion
libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test

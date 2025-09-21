val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala_lox",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= Seq(
      "-Wunused:all",
      "-Werror"
    ),

    // Check formatting in CI/compile
    Compile / compile := (Compile / compile).dependsOn(scalafmtCheckAll).value,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.typelevel" %% "cats-core" % "2.12.0"
    )
  )

val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finite-automata",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies +=
      "org.scalameta" %% "munit" % "0.7.26" % Test
  )

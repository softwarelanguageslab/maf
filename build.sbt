val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "maf2",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    // Cats 
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "mouse" % "1.2.1",
    libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.3.0",
    libraryDependencies += "org.typelevel" %% "cats-free" % "2.9.0",

    // Scala parsing
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",

    // Testing
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test",
  )

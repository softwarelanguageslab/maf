ThisBuild / scalaVersion := "3.3.7"

lazy val root = project
  .in(file("."))
  .aggregate(maf)

lazy val maf = project
  .in(file("code"))
  .settings(
    /** General settings */
    name := "maf",
    organization := "soft",
    version := "2.0",
    /** Dependencies */
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
    /** Compilation options */
    maxErrors := 5,
    /** Configuration for running the tests */
    Test / logBuffered := false,
    Test / testOptions += Tests.Argument("-oI"), // Produces a summary after running the tests, showing the failing tests
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test",
    libraryDependencies += "com.vladsch.flexmark" % "flexmark-all" % "0.62.2" % Test,
    /** General */
    Compile / mainClass := Some("maf.runners.Main"),
    run / fork := false,
    /**
     * Compiler options (see `scalac -help`, `scalac -W`, `scalac -X` and `scalac -Y`). We enable all the warnings of Scala 3.3 except
     * `-Wtostring-interpolated`, as the code interpolates values in strings on purpose (e.g., to implement `toString`), and make them errors.
     */
    scalacOptions ++= Seq(
      "-new-syntax", // Require `then` and `do` in control expressions
      "-Yexplicit-nulls", // Make reference types non-nullable
      "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros", // Allow macro definition (besides implementation and application)
      "-language:higherKinds", // Allow higher-kinded types
      "-language:implicitConversions", // Allow definition of implicit functions called views
      "-deprecation", // Warn about uses of deprecated APIs
      "-feature", // Warn about uses of features that should be imported explicitly
      "-unchecked", // Warn where generated code depends on assumptions (e.g., type tests that cannot be checked at run time)
      "-Wunused:all", // Warn about unused imports, private members, local definitions, parameters and pattern variables (see `@unused`)
      "-Wvalue-discard", // Warn when a non-Unit value is discarded because the expected type is Unit
      "-Wrecurse-with-default", // Warn when a method calls itself with a default argument
      "-Wenum-comment-discard", // Warn when a comment ambiguously assigned to multiple enum cases is discarded
      "-Xlint:all", // Warn when a class parameter shadows an inherited field, or a type parameter shadows a type in scope
      "-Werror" // Fail the compilation if there are any warnings
    ),
    // only for the main code, as the tests use the results of ScalaTest's `assert` and ScalaCheck's `property(...) = ...` as statements, and
    // register their tests while they are being initialized
    Compile / scalacOptions ++= Seq(
      "-Wnonunit-statement", // Warn when a statement of a block is a non-Unit expression
      "-Ysafe-init" // Warn when an object may be accessed before it is initialized
    ),
    Test / scalacOptions --= Seq("-Wnonunit-statement", "-Ysafe-init"),
    // allow unused definitions and imports in the REPL
    Compile / console / scalacOptions --= Seq("-Wunused:all", "-Werror")
  )

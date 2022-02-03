package maf.bench.scheme

object IncrementalSchemeBenchmarkPrograms:

    // Sequential -- ModF

    lazy val sequential: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme")(
      "qeval.scm", // define-syntax, force, delay
      "scheme.scm", // error in program
      "machine-simulator.scm", // map with three arguments
      "mountainvale.scm", // Too big...
      ".DS_Store"
    ) ++ cyclicValueReinforcement

    lazy val sequentialGenerated: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme/generated")(
      ".DS_Store",
      "fail.txt",
      "info.csv"
    )

    lazy val scam2020ModF: Set[String] = SchemeBenchmarkPrograms.toFolder("test/changes/scheme")(
      "leval.scm",
      "machine-simulator.scm",
      "mceval-dynamic.scm",
      "multiple-dwelling (coarse).scm",
      "multiple-dwelling (fine).scm",
      "nboyer.scm",
      "peval.scm"
    )

    lazy val assertions: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme/assertions")()

    // Small sample benchmarks that need CY.
    lazy val cyclicValueReinforcement: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/scheme/reinforcingcycles")()

    // Concurrent -- ModConc

    lazy val threads: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/changes/cscheme/threads")(
      "puzzle.scm", // Needs call-with-current-continuation.
      ".DS_Store"
    )
    lazy val concurrent: Set[String] = threads

    lazy val scam2020ModConc: Set[String] = SchemeBenchmarkPrograms.toFolder("test/changes/cscheme/threads")(
      "actors.scm",
      "crypt.scm",
      "crypt2.scm",
      "msort.scm",
      "pc.scm",
      "pps.scm",
      "stm.scm",
      "sudoku.scm",
      "mcarlo2.scm"
    )

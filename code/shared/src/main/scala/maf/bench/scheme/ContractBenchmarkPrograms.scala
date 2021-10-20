package maf.bench.scheme

object ContractBenchmarkPrograms:
    lazy val nguyenSafeOcty: Set[String] =
      SchemeBenchmarkPrograms.fromFolder("test/scv/NguyenGTH18/safe/octy")(
        ".DS_Store",
        "ex-03.rkt",
        "ex-08.rkt",
        "ex-09.rkt",
        "ex-11.rkt",
        "ex-12.rkt",
        "ex-13.rkt",
        "ex-14.rkt",
      )

    lazy val manualSafe: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/scv/manual/safe")(
      ".DS_Store",
    )

    lazy val manualUnsafe: Set[String] = SchemeBenchmarkPrograms.fromFolder("test/scv/manual/unsafe")(
      ".DS_Store"
    )

    lazy val allBenchmarks: Set[String] =
      nguyenSafeOcty ++ manualSafe

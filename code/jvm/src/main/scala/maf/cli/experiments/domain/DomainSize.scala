package maf.cli.experiments.domain

import maf.bench.scheme.*

object DomainSize:
    import SchemeBenchmarkPrograms.*
    val programs = ad ++ gabriel ++ gambit ++ scp1 ++ toplas98 ++ WCR2019

    def main(args: Array[String]): Unit =
        ???

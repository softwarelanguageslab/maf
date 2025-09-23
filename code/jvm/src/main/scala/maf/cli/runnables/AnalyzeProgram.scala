package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.SchemeAnalyses
import maf.core.{Identifier, Monad}
import maf.language.scheme.*
import maf.modular.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.modular.worklist.*
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.*

// null values are used here due to Java interop
import scala.language.unsafeNulls

object AnalyzeProgram extends App:
    def runAnalysis[A <: ModAnalysis[SchemeExp] with GlobalStore[SchemeExp]](bench: String, analysis: SchemeExp => A, timeout: () => Timeout.T): A =
        val text = SchemeParser.parseProgram(Reader.loadFile(bench))
        val a = analysis(text)
        print(s"Analysis of $bench ")
        try {
            val time = Timer.timeOnly {
                a.analyzeWithTimeout(timeout())
                println(a.store.filterNot(_.toString().contains("PrmAddr")).size)
                //println(a.program.prettyString())
            }
            println(s"terminated in ${time / 1000000} ms.")
            //a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) }).foreach(println)
        } catch {
            case t: Throwable =>
                println(s"raised exception.")
                System.err.println(t.getMessage)
                t.printStackTrace()
                System.err.flush()
        }
        a

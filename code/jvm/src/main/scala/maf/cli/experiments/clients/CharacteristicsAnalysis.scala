package maf.cli.experiments.clients

import maf.language.symbolic.lattices.*
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.modular.*
import maf.modular.worklist.*
import maf.modular.scheme.modf.*
import maf.modular.scheme.*
import maf.language.scheme.lattices.*
import maf.language.scheme.*
import maf.core.Address
import scala.reflect.ClassTag
import maf.util.Reader
import maf.util.benchmarks.{Table, Timeout}

/**
 * An analysis that computes certain characteristics of a program
 *
 * It measures:
 *
 * * The average (and total) number of calls to each component (not including itself) 
 * * The average (and total) number of calls to itself
 */

trait CharacteristicsAnalysis extends ModAnalysis[SchemeExp]:
    /** Set of calls from a component to another component */
    var calls: Set[(Component, Component)] = Set()

    /* Set of functions in the program */
    var functions: Set[Component] = Set() 

    override def intraAnalysis(component: Component): IntraCharacteristicsAnalysis

    trait IntraCharacteristicsAnalysis extends IntraAnalysis:

        override def spawn(cmp: Component): Unit =
            calls = calls + (this.component -> cmp)
            functions = functions + cmp
            super.spawn(cmp)

trait CharacteristicsAnalysisRunner extends ClientAnalysisRunner:
    override type Analysis <: CharacteristicsAnalysis
    override type Result = Option[Double]

    def setToMap[T](set: Set[(T, T)]): Map[T, List[T]] =
        set.foldLeft(Map[T, List[T]]()) { case (result, (from, to)) =>
            result + (from -> (to :: result.get(from).getOrElse(List())))
        }

    def computeAverageAndTotal(anl: Analysis, results: Set[(anl.Component, anl.Component)]): (Double, Double) =
        val asMap = setToMap(results)
        val numberOfComponents = asMap.keys.size
        val total = asMap.values.map(_.size).sum
        if numberOfComponents == 0 then (0, 0)
        else (total / numberOfComponents, numberOfComponents)

    def computeMaximumCallDepth(anl: Analysis, result: Set[(anl.Component, anl.Component)]): Double =

        import maf.util.MonoidImplicits.FoldMapExtension
        import maf.util.MonoidInstances
        import maf.util.Monoid
        given intMaxMonoid: Monoid[Int] = MonoidInstances.intMaxMonoid

        var visited: Set[anl.Component] = Set()
        def visit(calls: Map[anl.Component, List[anl.Component]], from: anl.Component, depth: Int): Int =
            if !visited.contains(from) && calls.contains(from) then
                visited += from
                calls(from).foldMap(to => visit(calls, to, depth + 1))
            else depth

        result.map(_._1).foldMap(visit(setToMap(result), _, 0)).toDouble

    override protected def runBenchmark(table: Table[Result], name: String): Table[Result] =
        val program = Reader.loadFile(name)
        val expr = parseProgram(program)
        val analysis = createAnalysis(expr)
        analysis.analyzeWithTimeout(timeout)
        var resultTable = table
        resultTable = toTable(Map(name -> results(analysis)), resultTable)
        for (f <- analysis.functions)
            resultTable = toTable(Map(s"$name ${f.toString()}" -> resultsPerComponent(analysis, f)), resultTable)
        resultTable

    def resultsPerComponent(analysis: Analysis, f: analysis.Component): Map[String, Result] =
        val filteredCalls = analysis.calls.filter(dep => dep._1 == f || dep._2 == f)
        val (callNoSelfAvg, callNoSelfTotal) = computeAverageAndTotal(analysis, filteredCalls.filter(dep => dep._1 != dep._2))
        val (callSelfAvg, callSelfTotal) = computeAverageAndTotal(analysis, filteredCalls.filter(dep => dep._1 == dep._2))

        val maximumDepth = computeMaximumCallDepth(analysis, filteredCalls)

        Map(
          "callDepsNoSelf" -> None,
          "callNoSelfTotal" -> Some(callNoSelfTotal),
          "callDepsSelf" -> None,
          "callSelfTotal" -> Some(callSelfTotal),
          "maxCallDepth" -> Some(maximumDepth)
        )

    def results(analysis: Analysis): Map[String, Result] =
        val (callNoSelfAvg, callNoSelfTotal) = computeAverageAndTotal(analysis, analysis.calls.filter(dep => dep._1 != dep._2))
        val (callSelfAvg, callSelfTotal) = computeAverageAndTotal(analysis, analysis.calls.filter(dep => dep._1 == dep._2))

        val maximumDepth = computeMaximumCallDepth(analysis, analysis.calls)

        Map(
          "callDepsNoSelf" -> Some(callNoSelfAvg),
          "callNoSelfTotal" -> Some(callNoSelfTotal),
          "callDepsSelf" -> Some(callSelfAvg),
          "callSelfTotal" -> Some(callSelfTotal),
          "maxCallDepth" -> Some(maximumDepth)
        )

object Characteristics extends CharacteristicsAnalysisRunner:
    type Analysis = CharacteristicsAnalysis

    // val benchmarks: Set[String] =
    //     (SchemeBenchmarkPrograms.sequentialBenchmarks.filterNot(p =>
    //         // undefiner issues (define in invalid context) TODO: check if this is the case or it is an error in the undefiner
    //         p.startsWith("test/R5RS/ad") || p.startsWith("test/R5RS/WeiChenRompf2019/the-little-schemer")
    //     ) -- Set(
    //       // also undefiner issues
    //       "test/R5RS/various/lambda-update.scm",
    //       "test/R5RS/scp1/car-counter.scm",
    //       "test/R5RS/scp1/twitter.scm",
    //       "test/R5RS/scp1/university.scm",
    //       "test/R5RS/various/strong-update.scm", // not sure what's wrong here? thought we fixed that.
    //     ))

    val benchmarks: Set[String] = Set("test/R5RS/gabriel/puzzle.scm")

    def createAnalysis(exp: SchemeExp): Analysis =
        new ModAnalysis[SchemeExp](exp)
            with StandardSchemeModFComponents
            with SchemeModFSemanticsM
            with SchemeModFNoSensitivity
            with BigStepModFSemantics
            with SymbolicSchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with CharacteristicsAnalysis:

            class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with IntraCharacteristicsAnalysis with BigStepModFIntra
            override def intraAnalysis(cmp: Component): AnalysisIntra =
                new AnalysisIntra(cmp)

    /** Parses the given program text to a SchemeExp */
    def parseProgram(txt: String): SchemeExp =
        SchemeParser.parseProgram(txt)
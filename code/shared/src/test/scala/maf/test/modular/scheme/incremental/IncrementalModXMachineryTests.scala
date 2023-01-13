package maf.test.modular.scheme.incremental

import maf.core.Address
import maf.language.CScheme.CSchemeParser
import maf.language.scheme.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.scheme.{SchemeConstantPropagationDomain, SchemeDomain}
import maf.modular.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.scheme.modconc.*
import maf.modular.incremental.scheme.modf.*
import maf.modular.scheme.modf.*
import maf.modular.scheme.ssmodconc.KKallocModConc
import maf.modular.worklist.*
import maf.test.*
import maf.util.Reader
import maf.util.benchmarks.Timeout
import org.scalatest.Tag
import org.scalatest.propspec.AnyPropSpec

import scala.concurrent.duration.*

class IncrementalModXMachineryTests extends AnyPropSpec:
    type Benchmark = String
    type Analysis = ModAnalysis[SchemeExp]
        with GlobalStore[SchemeExp]
        with ReturnValue[SchemeExp]
        with SchemeDomain
        with IncrementalModAnalysis[SchemeExp]

    private val fast: Set[String] = Set("baseline", "collatz", "nboyer", "satCoarse", "scheme")

    def testTags(b: Benchmark): Seq[Tag] = if fast.forall(f => !b.contains(f)) then Seq(IncrementalTest, SlowTest) else Seq(IncrementalTest)

    def parse(benchmark: Benchmark): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(benchmark))

    def sequentialAnalysis(e: SchemeExp): Analysis =
        new ModAnalysis[SchemeExp](e)
            with StandardSchemeModFComponents
            with SchemeModFCallSiteSensitivity
            with SchemeModFSemanticsM
            with LIFOWorklistAlgorithm[SchemeExp]
            with IncrementalSchemeModFBigStepSemantics
            with IncrementalSchemeTypeDomain
            with IncrementalGlobalStoreCY[SchemeExp] {
            var configuration: IncrementalConfiguration = ci_di_wi // allOptimisations
            override def intraAnalysis(
                cmp: Component
              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis
        }

    def parallelAnalysis(e: SchemeExp): Analysis =
        new ModAnalysis[SchemeExp](e)
            with KKallocModConc
            with IncrementalSchemeModConcSmallStepSemantics
            with LIFOWorklistAlgorithm[SchemeExp]
            with IncrementalSchemeTypeDomain
            with IncrementalGlobalStoreCY[SchemeExp] {

            val k = 0
            var configuration: IncrementalConfiguration = ci_di_wi // allOptimisations

            override def intraAnalysis(
                cmp: Component
              ) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreCYIntraAnalysis
        }

    def timeout(): Timeout.T = Timeout.start(Duration(5, MINUTES))

    def runAnalysis(makeAnalysis: SchemeExp => Analysis, benchmark: Benchmark): Analysis =
        val a = makeAnalysis(parse(benchmark))
        try
            val to = timeout()
            a.analyzeWithTimeout(to)
            assume(a.finished, s"Analysis of $benchmark timed out.")
        catch
            case e: VirtualMachineError =>
                System.gc()
                cancel(s"Analysis of $benchmark encountered an error: $e.")
        a

    /** Tests whether the analysis correctly infers components that should be reanalysed. */
    def testUpdatedComponents(): Unit =
        val expectedResults: List[(String, SchemeExp => Analysis, Set[String])] = List(
          ("test/changes/scheme/fib.scm", sequentialAnalysis, Set("λ@1:16 [[4:50]]", "λ@1:16 [[4:23]]", "λ@1:16 [[5:4]]")),
          ("test/changes/scheme/satRem.scm", sequentialAnalysis, Set("λ@15:23 [[11:8]]", "main", "λ@15:23 [[11:15]]")),
          ("test/changes/scheme/ring-rotate.scm", sequentialAnalysis, Set("main", "print-ring [[35:2]]")),
          ("test/changes/cscheme/threads/actors.scm", parallelAnalysis, Set("ThreadComponent((act state name), ())")),
          ("test/changes/cscheme/threads/mcarlo.scm", parallelAnalysis, Set("main"))
        )
        expectedResults.foreach { case (benchmark, makeAnalysis, affected) =>
            property(s"Incremental analysis finds correct updated expressions for $benchmark.", testTags(benchmark): _*) {
                val a = runAnalysis(makeAnalysis, benchmark)
                val directlyAffected: Set[String] = a.findUpdatedExpressions(a.program).flatMap(a.mapping).map(_.toString)
                assert(directlyAffected.size == affected.size)
                assert(directlyAffected.forall(affected.contains), s" - Directly affected component mismatch.")
            }
        }

    /** Tests whether the deletion of components works correctly using an artificial code example. */
    def testComponentDeletion(): Unit =
        // Artificial program.
        val program: String =
            """(define (a)
          |  (<change> (b) #t)
          |  (d))
          |(define (b)
          |  (c)
          |  (a)
          |  (e))
          |(define (c)
          |  (<change> (b) #t)
          |  (d)
          |  (<change> (e) #t))
          |(define (d)
          |  (<change> (e) #t))
          |(define (e)
          |  (<change> #t (d))
          |  (b))
          |(a)""".stripMargin

        // Base analysis.
        val base: Analysis = new ModAnalysis[SchemeExp](CSchemeParser.parseProgram(program))
            with StandardSchemeModFComponents
            with SchemeModFNoSensitivity
            with SchemeModFSemanticsM
            with LIFOWorklistAlgorithm[SchemeExp]
            with IncrementalSchemeModFBigStepSemantics
            with IncrementalSchemeConstantPropagationDomain
            with IncrementalGlobalStoreCY[SchemeExp] {
            var configuration: IncrementalConfiguration = allOptimisations
            override def intraAnalysis(
                cmp: Component
              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis
        }

        // Expected results.
        val exp1 = Set("main", "a [ε]", "b [ε]", "c [ε]") // main, a, b, c. Not d, e because the analysis should stop at bottom.
        val exp2 = Set("main", "a [ε]", "d [ε]") // main, a, d.

        // Actual test.
        for c <- allConfigurations.filter(_.componentInvalidation) do
            property(s"Component invalidation works on an artificial example using ${c}.", IncrementalTest) {
                try
                    val a = base.deepCopy()
                    a.configuration = c
                    a.analyzeWithTimeout(timeout())
                    assume(a.finished)
                    assert(a.visited.map(_.toString) == exp1)
                    a.updateAnalysis(timeout())
                    // a.deleteDisconnectedComponents() // Normally performed automatically by CI.
                    assert(a.visited.map(_.toString) == exp2)
                catch
                    case e: VirtualMachineError =>
                        System.gc()
                        cancel(s"Analysis of program encountered an error: $e.")
            }

    /** Tests whether the deletion of dependencies works correctly using an artificial code example. */
    def testDependencyDeletion(): Unit =
        val program: String =
            """(define (f a b c d e)
      |  (<change> (+ a b) (e))
      |  (c d)
      |  (<change> (e) (h (lambda (a b c d e) (d (lambda (t g f r d) (if (> t g) f (lambda (a b c d e) 3))))))))
      |(define (g a)
      |  (a (lambda (a b c d e) #f)))
      |(define (j)
      |  (<change> #t (list f g h)))
      |(define (h fn)
      |  (if (number? fn)
      |      (<change> (g 1) (h (lambda (a b c d e) 2)))
      |      (fn 1 0 g h j)))
      |(h f)""".stripMargin

        val base: Analysis = new ModAnalysis[SchemeExp](CSchemeParser.parseProgram(program))
            with StandardSchemeModFComponents
            with SchemeModFCallSiteSensitivity
            with SchemeModFSemanticsM
            with FIFOWorklistAlgorithm[SchemeExp]
            with IncrementalSchemeModFBigStepSemantics
            with IncrementalSchemeConstantPropagationDomain
            with IncrementalGlobalStoreCY[SchemeExp]
            //with IncrementalLogging[SchemeExp]
            {
            // override def focus(a: Addr): Boolean = !a.toString.toLowerCase().nn.contains("prm")
            var configuration: IncrementalConfiguration = ci_di_wi // allOptimisations
            override def intraAnalysis(
                cmp: Component
              ) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis //with IncrementalLoggingIntra
        }

        def getStandard(p: SchemeExp) = new ModAnalysis[SchemeExp](p)
            with StandardSchemeModFComponents
            with SchemeModFCallSiteSensitivity
            with SchemeModFSemanticsM
            with FIFOWorklistAlgorithm[SchemeExp]
            with BigStepModFSemantics
            with SchemeConstantPropagationDomain
            with GlobalStore[SchemeExp] {
            override def intraAnalysis(cmp: SchemeModFComponent) = new IntraAnalysis(cmp) with BigStepModFIntra with GlobalStoreIntra
        }

        // Expected results.
        val exp1 =
            val a = getStandard(ProgramVersionExtracter.getInitial(CSchemeParser.parseProgram(program)))
            a.analyzeWithTimeout(timeout())
            if a.finished then a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) })
            else ""
        val exp2 =
            val a = getStandard(ProgramVersionExtracter.getUpdated(CSchemeParser.parseProgram(program)))
            a.analyzeWithTimeout(timeout())
            if a.finished then a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) })
            else ""
        /* val exp3 = exp2 ++ Set(
      "(AddrDep(VarAddr(h@9:10 [None])),j [[4:14]])",
      "(AddrDep(VarAddr(f@1:10 [None])),j [[4:14]])",
      "(AddrDep(VarAddr(g@5:10 [None])),j [[4:14]])",
      "(AddrDep(PrmAddr(list)),j [[4:14]])"
    )
         */

        // Actual test. Note that component invalidation lowers the number of dependencies and is hence required (expected results obtained by performing a full analysis on both program versions).
        for c <- allConfigurations.filter(c => c.dependencyInvalidation && c.componentInvalidation) do
            property(s"Dependency invalidation works on an artificial example using ${c}.", IncrementalTest) {
                try
                    assume(exp1 != "")
                    assume(exp2 != "")
                    val a = base.deepCopy()
                    a.configuration = c
                    a.analyzeWithTimeout(timeout())
                    assume(a.finished)
                    val d1 = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) })
                    assert(d1 == exp1)
                    a.updateAnalysis(timeout())
                    val d2 = a.deps.toSet[(Dependency, Set[a.Component])].flatMap({ case (d, cmps) => cmps.map(c => (d, c).toString()) })
                    assert(d2 == exp2) // (if (c.componentInvalidation) exp2 else exp3))
                catch
                    case e: VirtualMachineError =>
                        System.gc()
                        cancel(s"Analysis of program encountered an error: $e.")
            }

    def testComplexSCADetection() =

        type A = Analysis with IncrementalGlobalStoreCY[SchemeExp]

        def analysis(p: SchemeExp): A = new ModAnalysis[SchemeExp](p)
            with StandardSchemeModFComponents
            with SchemeModFCallSiteSensitivity
            with SchemeModFSemanticsM
            with FIFOWorklistAlgorithm[SchemeExp]
            with IncrementalSchemeModFBigStepSemantics
            with IncrementalSchemeConstantPropagationDomain
            with IncrementalGlobalStoreCY[SchemeExp]
            //with IncrementalLogging[SchemeExp]
        {
            // override def focus(a: Addr): Boolean = !a.toString.toLowerCase().nn.contains("prm")
            var configuration: IncrementalConfiguration = allOptimisations

            override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreCYIntraAnalysis //with IncrementalLoggingIntra
        }

        // More test programs: use /test/taint folder but remove the taint annotations.
        val p1: String =
            // Cycle: x -[explicit]-> y -[implicit]-> x
            """(define x 0)
                |(define (f) (g))
                |(define (g) (set! x #t))
                |(define (h y)
                |  (if (= y 0)
                |      (f)))
                |(h x)""".stripMargin
        def c1(SCAs: Set[Set[Address]]): Unit = assert(SCAs.exists(sca => sca.exists(a => a.toString.contains("x@1:9")) && sca.exists(a => a.toString.contains("y@4:12"))))

        val p2: String = // tainted-function-choice-2
          """(define a #t)
              |(define (b x) x)
              |(define (set-b)
              |  (set! b (lambda (x) #f)))
              |(if a (set-b))
              |(define res (b 10))
              |res""".stripMargin
        def c2(SCAs: Set[Set[Address]]): Unit = assert(SCAs.isEmpty)

        val tests = List(
            (p1, c1, "p1"),
            (p2, c2, "p2")
        ).map(t => (CSchemeParser.parseProgram(t._1), t._2, t._3))

        tests.foreach { case (p, c, n) =>
            property(s"Complex implicit cycle is found for constructed program $n", IncrementalTest) {
                val a = analysis(p)
                a.analyzeWithTimeout(timeout())
                assume(a.finished)
                val SCAs = a.computeSCAs()
                println(a.program.prettyString())
                println(a.explicitAndIntraComponentImplicitFlowsR())
                println(a.computeTransitiveInterComponentFlows())
                println(SCAs)
                c(SCAs)
            }
        }
    end testComplexSCADetection

   // testComponentDeletion()
   // testDependencyDeletion()
   // testUpdatedComponents()
    testComplexSCADetection()

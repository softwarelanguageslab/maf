package maf.runners.precision

import maf.analysis.*
import maf.domain.*
import maf.domain.lattice.*
import maf.domain.scheme.*
import maf.fixpoint.*
import maf.runners.precision.address.*
import maf.semantics.scheme.interpreter.{ConcreteValues, EmptyIO, FileIO, IO, SchemeInterpreter}
import maf.syntax.*
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.Preluded
import maf.util.*

abstract class PrecisionBenchmarks[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]:

    type Benchmark = String
    type Analysis = AnalysisEntry[SchemeExp[Named]] with AnalysisResults[SchemeExp[Any]] with ModularSchemeDomain {
        val modularLatticeWrapper: ModularSchemeLatticeWrapper {
            val modularLattice: ModularSchemeLattice[Address, Str, Bln, Num, Rea, Chr, Smb]
        }
    }

    implicit def cpAnalysis(anl: ModularSchemeDomain): Analysis = anl.asInstanceOf[Analysis]

    private def convertAddr(addr: Address): BaseAddr = addr match
        case maf.domain.scheme.VarAddr(vrb, _) => address.VarAddr(vrb)
        case maf.domain.scheme.PtrAddr(exp, _) => address.PtrAddr(exp)
        case maf.domain.scheme.PrmAddr(nam)    => address.PrmAddr(nam)

    val baseDomain = new ModularSchemeLattice[BaseAddr, Str, Bln, Num, Rea, Chr, Smb]
    val baseLattice = baseDomain.schemeLattice
    type BaseValue = baseDomain.L

    val emptyEnv = Environment[BaseAddr](Iterable.empty)
    protected def convertV(analysis: Analysis)(value: analysis.modularLatticeWrapper.modularLattice.Value): baseDomain.Value = value match
        case analysis.modularLatticeWrapper.modularLattice.Nil          => baseDomain.Nil
        case analysis.modularLatticeWrapper.modularLattice.Bool(b)      => baseDomain.Bool(b)
        case analysis.modularLatticeWrapper.modularLattice.Int(i)       => baseDomain.Int(i)
        case analysis.modularLatticeWrapper.modularLattice.Real(r)      => baseDomain.Real(r)
        case analysis.modularLatticeWrapper.modularLattice.Char(c)      => baseDomain.Char(c)
        case analysis.modularLatticeWrapper.modularLattice.Str(s)       => baseDomain.Str(s)
        case analysis.modularLatticeWrapper.modularLattice.Symbol(s)    => baseDomain.Symbol(s)
        case analysis.modularLatticeWrapper.modularLattice.Prim(ps)     => baseDomain.Prim(ps)
        case analysis.modularLatticeWrapper.modularLattice.Clo(cs)      => baseDomain.Clo(cs.map(c => (c._1, emptyEnv)))
        case analysis.modularLatticeWrapper.modularLattice.Cons(a, d)   => baseDomain.Cons(convertValue(analysis)(a), convertValue(analysis)(d))
        case analysis.modularLatticeWrapper.modularLattice.Pointer(ps)  => baseDomain.Pointer(ps.map(convertAddr))
        case analysis.modularLatticeWrapper.modularLattice.Vec(s, e)    => baseDomain.Vec(s, e.view.mapValues(convertValue(analysis)).toMap)
        case analysis.modularLatticeWrapper.modularLattice.Void         => baseDomain.Void
        case v                                                          => throw new Exception(s"Unsupported value type for conversion: ${v.ord}.")

    protected def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue =
        baseLattice.join(value.elements[analysis.modularLatticeWrapper.modularLattice.Value].map(v => baseDomain.Element(convertV(analysis)(v))))

    private def convertConcreteAddr(addr: ConcreteValues.Addr): BaseAddr = addr._2 match
        case ConcreteValues.AddrInfo.VarAddr(v) => address.VarAddr(v)
        case ConcreteValues.AddrInfo.PtrAddr(p) => address.PtrAddr(p)
        case ConcreteValues.AddrInfo.PrmAddr(p) => address.PrmAddr(p)

    protected def convertConcreteValue(value: ConcreteValues.Value): BaseValue = value match
        case ConcreteValues.Value.Nil          => baseLattice.nil
        case ConcreteValues.Value.Void         => baseLattice.void
        case ConcreteValues.Value.Undefined(_) => baseLattice.bottom
        case ConcreteValues.Value.Clo(l, _)    => baseLattice.closure((l, emptyEnv))
        case ConcreteValues.Value.Primitive(p) => baseLattice.primitive(p)
        case ConcreteValues.Value.Str(s)       => baseLattice.string(s)
        case ConcreteValues.Value.Symbol(s)    => baseLattice.symbol(s)
        case ConcreteValues.Value.Integer(i)   => baseLattice.number(i)
        case ConcreteValues.Value.Real(r)      => baseLattice.real(r)
        case ConcreteValues.Value.Bool(b)      => baseLattice.bool(b)
        case ConcreteValues.Value.Character(c) => baseLattice.char(c)
        case ConcreteValues.Value.Cons(a, d)   => baseLattice.cons(convertConcreteValue(a), convertConcreteValue(d))
        case ConcreteValues.Value.Pointer(a)   => baseLattice.pointer(convertConcreteAddr(a))
        case ConcreteValues.Value.Vector(siz, els, _) =>
            def convertNumber(n: BigInt): Num = IntLattice[Num].inject(n)

            val cSiz = convertNumber(siz)
            val cEls = els.foldLeft(Map[Num, BaseValue]()) { case (acc, (idx, vlu)) =>
                val cIdx = convertNumber(idx)
                val cVlu = convertConcreteValue(vlu)
                val prevVlu = acc.getOrElse(cIdx, baseLattice.bottom)
                val newVlu = baseLattice.join(cVlu, prevVlu)
                acc + (cIdx -> newVlu)
            }
            baseDomain.Element(baseDomain.Vec(cSiz, cEls))
        case v => throw new Exception(s"Unsupported value for concrete conversion: $v")

    // compares the results of concrete and/or abstract interpreters using a `ResultMap`
    // a ResultMap which is a mapping from positions to a (context-insensitve) value
    type ResultMap = Map[Option[Position], BaseValue]

    // running an analysis results in either
    // - succesful termination (meaning the ResultMap `res` is sound)
    // - a timeout (where `res` are the partial -- i.e., potentially unsound -- results)
    // - an error (where `exc` is the exception or VM error that was thrown)
    sealed trait AnalysisResult
    case class Terminated(res: ResultMap) extends AnalysisResult
    case class TimedOut(res: ResultMap) extends AnalysisResult
    case class Errored(err: Exception | VirtualMachineError) extends AnalysisResult

    /**
     * Compare the precision of any two ResultMaps r1 and r2
     * @param r1
     *   a ResultMap
     * @param r2
     *   a ResultMap
     * @return
     *   a quadruple of:
     *   - the set of program locations where abstract values have remained unchanged
     *   - the set of program locations where abstract values are more precise in r2
     *   - the set of program locations where abstract values are less precise in r2
     *   - the set of program locations where abstract values are not comparable between r1 and r2
     */
    protected def compare(b1: ResultMap, b2: ResultMap): (Set[Option[Position]], Set[Option[Position]], Set[Option[Position]], Set[Option[Position]]) =
        val allKeys = b1.keySet ++ b2.keySet
        allKeys.foldLeft((Set.empty[Option[Position]], Set.empty[Option[Position]], Set.empty[Option[Position]], Set.empty[Option[Position]])) { (acc, pos) =>
            val value1 = b1.getOrElse(pos, baseLattice.bottom)
            val value2 = b2.getOrElse(pos, baseLattice.bottom)
            if value1 == value2 then (acc._1 + pos, acc._2, acc._3, acc._4)
            else if baseLattice.subsumes(value1, value2) then 
                    (acc._1, acc._2 + pos, acc._3, acc._4)
            else if baseLattice.subsumes(value2, value1) then 
                    (acc._1, acc._2, acc._3 + pos, acc._4)
            else // neither value is more precise than the other
                    (acc._1, acc._2, acc._3, acc._4 + pos)
        }

    /**
     * Compare two ResultMaps, assuming one is more precise than the other
     * @param b1
     *   the less precise resultMap
     * @param b2
     *   the more precise resultMap
     * @param check
     *   a boolean indicating whether it should be checked explicitly that b1 is less precise than b2 (default: true)
     * @return
     *   the set of addresses that have been refined in b2 w.r.t. b1
     */
    protected def compareOrdered(r1: ResultMap, r2: ResultMap, check: Boolean = true): Set[Option[Position]] =
        def errorMessage(pos: Option[Position]): String =
            val value1 = r1.getOrElse(pos, baseLattice.bottom)
            val value2 = r2.getOrElse(pos, baseLattice.bottom)
            s"""
              | At addr ${pos.show}: value v2 of r2 is not subsumed by value v1 of r1.
              | where v1 = $value1
              |       v2 = $value2 
            """.stripMargin
        val (_, morePrecise, lessPrecise, unrelated) = compare(r1, r2)
        if check then
            assert(lessPrecise.isEmpty, errorMessage(lessPrecise.head))
            assert(unrelated.isEmpty, errorMessage(unrelated.head))
        morePrecise

    /**
     * Given an analysis (that terminated), extract its ResultMap
     * @param analysis
     *   the analysis from which the results need to be extracted
     * @return
     *   a ResultMap containing the context-insensitive results for that analysis
     */
    protected def extract(analysis: Analysis): ResultMap =
        analysis.resultsPerPos.view
            .mapValues(vs => analysis.lattice.join(vs))
            .mapValues(convertValue(analysis))
            .toMap

    /**
     * Run the analysis on a given program
     *
     * @param analysis
     *   a function that creates an analysis for a given program
     * @param program
     *   the program to analyze
     * @param name
     *   the name for the analysis (used for reporting to the console)
     * @param path
     *   the name of / path to the benchmark program to run
     * @param timeout
     *   (optional) the timeout
     * @return
     *   an option value, being:
     *   - the base store if the analysis terminated
     *   - `None` otherwise
     */
    protected def runAnalysis(
        analysis: SchemeExp[Named] & Preluded => Analysis,
        name: String,
        program: SchemeExp[Named] & Preluded,
        path: Benchmark,
        timeout: Timeout.T = Timeout.none
      ): AnalysisResult =
        try
            val anl = analysis(program)
            println(s"... analysing $path using $name ...")
            anl.analyzeWithTimeout(timeout)
            val res = extract(anl)
            if anl.finished then Terminated(res)
            else TimedOut(res)
        catch
            case e: Exception =>
                e.printStackTrace()
                println(s"Analyzer failed with exception $e")
                Errored(e)
            case e: VirtualMachineError =>
                System.gc()
                println(s"Analyzer failed with error $e")
                Errored(e)

    /**
     * Create a concrete interpeter.
     *
     * @param addResult
     *   a callabck function that is called when the store is updated in the concrete interpeter
     * @param io
     *   a mock FileIO class that is used when the concrete interpreter requires some input
     */
    def createInterpreter(addResult: (Option[Position], ConcreteValues.Value) => Unit, io: IO = new EmptyIO()): SchemeInterpreter =
        new SchemeInterpreter(addResult, io)

    /**
     * Hook that gets executed when the interpreter throws an exception.
     *
     * It rethrows the exception as a default behaviour
     *
     * @param addResult
     *   a callback function that can register values in the result map
     */
    def handleInterpreterError(addResult: (Option[Position], ConcreteValues.Value) => Unit): PartialFunction[Throwable, Any] = { case e => throw e }

    /**
     * Run the concrete interpreter on a given program
     *
     * @param program
     *   the Scheme program to run the interpreter on
     * @param path
     *   the name of / path to the benchmark program to run
     * @param timeout
     *   (optional) the timeout
     * @param times
     *   how many times the interpreter needs to run, results are joined together
     * @return
     *   an option value, being:
     *   - the base (joined) store if the interpreter (always) terminated
     *   - `None` otherwise
     */
    protected def runInterpreter(
        program: SchemeExp[Named] & Preluded,
        path: Benchmark,
        timeout: Timeout.T = Timeout.none,
        times: Int = 1
      ): Option[ResultMap] =
        print(s"Running concrete interpreter on $path ($times times)")
        var posResults: ResultMap = Map.empty.withDefaultValue(baseLattice.bottom)
        def addResult(pos: Option[Position], vlu: ConcreteValues.Value) =
            posResults += pos -> (baseLattice.join(posResults(pos), convertConcreteValue(vlu)))
        try
            for _ <- 1 to times do
                print(".")
                val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")))
                try interpreter.run(program, timeout)
                catch handleInterpreterError(addResult)
            println()
            Some(posResults)
        catch
            case e: Exception =>
                println(s"Concrete interpreter failed with $e")
                None
            case e: VirtualMachineError =>
                System.gc()
                println(s"Concrete interpreter failed with $e")
                None

    /**
     * Specify what needs to be done for a given benchmark program
     * @param path
     *   the path to the benchmark program
     * @param program
     *   the Scheme expression of the entire program
     */
    protected def forBenchmark(path: Benchmark, program: SchemeExp[Named] & Preluded): Unit

    /** Parses a program into the form that the analyses support */
    protected def parseProgram(txt: String, source: Source): SchemeExp[Named] & Preluded

    /**
     * Run a benchmark
     * @param benchmark
     *   the benchmark program to run
     */
    def runBenchmark(benchmark: Benchmark) =
        val txt = Reader.loadFile(benchmark)
        val prg = parseProgram(txt, Source.File(benchmark))
        forBenchmark(benchmark, prg)

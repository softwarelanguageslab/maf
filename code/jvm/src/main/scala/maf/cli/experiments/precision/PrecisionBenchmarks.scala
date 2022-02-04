package maf.cli.experiments.precision

import maf.core._
import maf.language.scheme._
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, IO, SchemeInterpreter}
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.language.scheme.primitives._
import maf.lattice.interfaces._
import maf.modular._
import maf.modular.scheme._
import maf.util._
import maf.util.benchmarks.Timeout
import maf.language.scheme.interpreter.EmptyIO

abstract class PrecisionBenchmarks[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]:

    type Benchmark = String
    type Analysis = AnalysisEntry[SchemeExp] with AnalysisResults[SchemeExp] with ModularSchemeDomain {
      val modularLatticeWrapper: ModularSchemeLatticeWrapper {
        val modularLattice: ModularSchemeLattice[Address, Str, Bln, Num, Rea, Chr, Smb]
      }
    }

    implicit def cpAnalysis(anl: ModularSchemeDomain): Analysis = anl.asInstanceOf[Analysis]

    sealed trait BaseAddr extends Address { def printable = true; def idn: Identity }
    case class VarAddr(vrb: Identifier) extends BaseAddr { def idn: Identity = vrb.idn; override def toString = s"<variable $vrb>" }
    case class PtrAddr(exp: Expression) extends BaseAddr { def idn = exp.idn; override def toString = s"<pointer $exp>" }
    case class PrmAddr(nam: String) extends BaseAddr { def idn: Identity = Identity.none; override def toString = s"<primitive $nam>" }

    private def convertAddr(addr: Address): BaseAddr = addr match
        case maf.modular.scheme.VarAddr(vrb, _) => VarAddr(vrb)
        case maf.modular.scheme.PtrAddr(exp, _) => PtrAddr(exp)
        case maf.modular.scheme.PrmAddr(nam)    => PrmAddr(nam)

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
        case analysis.modularLatticeWrapper.modularLattice.Lock(tids)   => baseDomain.Lock(tids)
        case analysis.modularLatticeWrapper.modularLattice.Thread(tids) => baseDomain.Thread(tids)
        case v                                                          => throw new Exception(s"Unsupported value type for conversion: ${v.ord}.")

    protected def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue = value match
        case analysis.modularLatticeWrapper.modularLattice.Elements(vs) => baseDomain.Elements(vs.map(convertV(analysis)))

    private def convertConcreteAddr(addr: ConcreteValues.Addr): BaseAddr = addr._2 match
        case ConcreteValues.AddrInfo.VarAddr(v) => VarAddr(v)
        case ConcreteValues.AddrInfo.PtrAddr(p) => PtrAddr(p)
        case ConcreteValues.AddrInfo.PrmAddr(p) => PrmAddr(p)

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
          def convertNumber(n: BigInt): Num = baseLattice.number(n) match
              case baseDomain.Elements(vs) => vs.head.asInstanceOf[baseDomain.Int].i

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
    // a ResultMap which is a mapping from identities to a (context-insensitve) value
    type ResultMap = Map[Identity, BaseValue]

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
    protected def compare(b1: ResultMap, b2: ResultMap): (Set[Identity], Set[Identity], Set[Identity], Set[Identity]) =
        val allKeys = b1.keySet ++ b2.keySet
        allKeys.foldLeft((Set.empty[Identity], Set.empty[Identity], Set.empty[Identity], Set.empty[Identity])) { (acc, pos) =>
            val value1 = b1.getOrElse(pos, baseLattice.bottom)
            val value2 = b2.getOrElse(pos, baseLattice.bottom)
            if value1 == value2 then (acc._1 + pos, acc._2, acc._3, acc._4)
            else if baseLattice.subsumes(value1, value2) then (acc._1, acc._2 + pos, acc._3, acc._4)
            else if baseLattice.subsumes(value2, value1) then (acc._1, acc._2, acc._3 + pos, acc._4)
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
    protected def compareOrdered(r1: ResultMap, r2: ResultMap, check: Boolean = true): Set[Identity] =
        def errorMessage(pos: Identity): String =
            val value1 = r1.getOrElse(pos, baseLattice.bottom)
            val value2 = r2.getOrElse(pos, baseLattice.bottom)
            s"""
              | At addr $pos: value v2 of r2 is not subsumed by value v1 of r1.
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
      analysis.resultsPerIdn.view
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
        analysis: SchemeExp => Analysis,
        name: String,
        program: SchemeExp,
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
    def createInterpreter(addResult: (Identity, ConcreteValues.Value) => Unit, io: IO = new EmptyIO()): SchemeInterpreter =
      new SchemeInterpreter(addResult, io)

    /**
     * Hook that gets executed when the interpreter throws an exception.
     *
     * It rethrows the exception as a default behaviour
     *
     * @param addResult
     *   a callback function that can register values in the result map
     */
    def handleInterpreterError(addResult: (Identity, ConcreteValues.Value) => Unit): PartialFunction[Throwable, Any] = { case e => throw e }

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
        program: SchemeExp,
        path: Benchmark,
        timeout: Timeout.T = Timeout.none,
        times: Int = 1
      ): Option[ResultMap] =
        print(s"Running concrete interpreter on $path ($times times)")
        var idnResults: ResultMap = Map.empty.withDefaultValue(baseLattice.bottom)
        def addResult(idn: Identity, vlu: ConcreteValues.Value) =
          idnResults += idn -> (baseLattice.join(idnResults(idn), convertConcreteValue(vlu)))
        try
            for _ <- 1 to times do
                print(".")
                val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")))
                try interpreter.run(program, timeout)
                catch handleInterpreterError(addResult)
            println()
            Some(idnResults)
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
    protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit

    protected def parseProgram(txt: String): SchemeExp = SchemeParser.parseProgram(txt)

    /**
     * Run a benchmark
     * @param benchmark
     *   the benchmark program to run
     */
    def runBenchmark(benchmark: Benchmark) =
        val txt = Reader.loadFile(benchmark)
        val prg = parseProgram(txt)
        forBenchmark(benchmark, prg)

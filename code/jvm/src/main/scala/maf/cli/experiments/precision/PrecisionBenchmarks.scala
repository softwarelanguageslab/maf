package maf.cli.experiments.precision

import maf.core._
import maf.language.scheme._
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, SchemeInterpreter}
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.language.scheme.primitives._
import maf.lattice.interfaces._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.util._
import maf.util.benchmarks.Timeout

abstract class PrecisionBenchmarks[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice] {

  type Benchmark = String
  type Analysis = ModAnalysis[SchemeExp] with BaseSchemeModFSemantics with ModularSchemeDomain {
    val modularLatticeWrapper: ModularSchemeLatticeWrapper {
      val modularLattice: ModularSchemeLattice[Addr, Str, Bln, Num, Rea, Chr, Smb]
    }
  }

  sealed trait BaseAddr extends Address { def printable = true }
  case class VarAddr(vrb: Identifier) extends BaseAddr { def idn: Identity = vrb.idn; override def toString = s"<variable $vrb>" }
  case class PrmAddr(nam: String) extends BaseAddr { def idn: Identity = Identity.none; override def toString = s"<primitive $nam>" }
  case class RetAddr(idn: Identity) extends BaseAddr { override def toString = s"<return $idn>" }
  case class PtrAddr(idn: Identity) extends BaseAddr { override def toString = s"<pointer $idn>" }

  private def convertAddr(analysis: Analysis)(addr: analysis.Addr): BaseAddr = addr match {
    case maf.modular.scheme.VarAddr(vrb, _) => VarAddr(vrb)
    case maf.modular.scheme.PtrAddr(exp, _) => PtrAddr(exp.idn)
    case maf.modular.ReturnAddr(_, idn)     => RetAddr(idn)
    case maf.modular.scheme.PrmAddr(nam)    => PrmAddr(nam)
    case a                                  => throw new Exception(s"Cannot convert address: $a")
  }

  type BaseValue = baseDomain.L
  val baseDomain = new ModularSchemeLattice[BaseAddr, Str, Bln, Num, Rea, Chr, Smb]
  val baseLattice = baseDomain.schemeLattice
  case class StubPrimitive(name: String) extends SchemePrimitive[BaseValue, BaseAddr] {
    def call(
        fpos: SchemeExp,
        args: List[(SchemeExp, BaseValue)],
        store: Store[BaseAddr, BaseValue],
        scheme: SchemeInterpreterBridge[BaseValue, BaseAddr]
      ) =
      throw new Exception("Stub primitive: call not supported")
  }
  val emptyEnv = Environment[BaseAddr](Iterable.empty)
  private def convertV(analysis: Analysis)(value: analysis.modularLatticeWrapper.modularLattice.Value): baseDomain.Value = value match {
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
    case analysis.modularLatticeWrapper.modularLattice.Pointer(ps)  => baseDomain.Pointer(ps.map(convertAddr(analysis)(_)))
    case analysis.modularLatticeWrapper.modularLattice.Vec(s, e)    => baseDomain.Vec(s, e.view.mapValues(convertValue(analysis)).toMap)
    case analysis.modularLatticeWrapper.modularLattice.Void         => baseDomain.Void
    case analysis.modularLatticeWrapper.modularLattice.Lock(tids)   => baseDomain.Lock(tids)
    case analysis.modularLatticeWrapper.modularLattice.Thread(tids) => baseDomain.Thread(tids)
    case v                                                          => throw new Exception(s"Unsupported value type for conversion: ${v.ord}.")
  }

  private def convertValue(analysis: Analysis)(value: analysis.Value): BaseValue = value match {
    case analysis.modularLatticeWrapper.modularLattice.Elements(vs) => baseDomain.Elements(vs.map(convertV(analysis)))
  }

  private def convertConcreteAddr(addr: ConcreteValues.Addr): BaseAddr = addr._2 match {
    case ConcreteValues.AddrInfo.VarAddr(v) => VarAddr(v)
    case ConcreteValues.AddrInfo.PrmAddr(p) => PrmAddr(p)
    case ConcreteValues.AddrInfo.PtrAddr(p) => PtrAddr(p.idn)
    case ConcreteValues.AddrInfo.RetAddr(r) => RetAddr(r.idn)
  }

  private def convertConcreteValue(value: ConcreteValues.Value): BaseValue = value match {
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
      def convertNumber(n: BigInt): Num = baseLattice.number(n) match {
        case baseDomain.Elements(vs) => vs.head.asInstanceOf[baseDomain.Int].i
      }

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
  }

  type BaseStore = Map[BaseAddr, BaseValue]

  /**
   * Joins two stores
   * @param b1
   *   a base store
   * @param b2
   *   a base store
   * @return
   *   the joined base store
   */
  protected def join(b1: BaseStore, b2: BaseStore): BaseStore =
    b2.foldLeft(b1) { case (acc, (addr2, value2)) =>
      val value1 = acc.getOrElse(addr2, baseLattice.bottom)
      val joined = baseLattice.join(value1, value2)
      acc + (addr2 -> joined)
    }

  /**
   * Compare two stores, assuming one is more precise than the other
   * @param b1
   *   the less precise store
   * @param b2
   *   the more precise store
   * @return
   *   the set of addresses that have been refined in b2 w.r.t. b1
   */
  protected def compareOrdered(b1: BaseStore, b2: BaseStore): Set[BaseAddr] = {
    def errorMessage(addr: BaseAddr): String = {
      val value1 = b1.getOrElse(addr, baseLattice.bottom)
      val value2 = b2.getOrElse(addr, baseLattice.bottom)
      s"""
        | At addr $addr: value v2 of b2 is not subsumed by value v1 of b1.
        | where v1 = $value1
        |       v2 = $value2 
      """.stripMargin
    }
    val (_, morePrecise, lessPrecise, unrelated) = compare(b1, b2)
    assert(lessPrecise.isEmpty, errorMessage(lessPrecise.head))
    assert(unrelated.isEmpty, errorMessage(unrelated.head))
    morePrecise
  }

  /**
   * Compare the precision of any two stores b1 and b2
   * @param b1
   *   a base store
   * @param b2
   *   a base store
   * @return
   *   a quadruple of:
   *   - the set of addresses whose abstract values have remained unchanged
   *   - the set of addresses whose abstract values are more precise in b2
   *   - the set of addresses whose abstract values are less precise in b2
   *   - the set of addresses whose abstract values are not comparable between b1 and b2
   */
  protected def compare(b1: BaseStore, b2: BaseStore): (Set[BaseAddr], Set[BaseAddr], Set[BaseAddr], Set[BaseAddr]) = {
    val allKeys = b1.keySet ++ b2.keySet
    allKeys.foldLeft((Set.empty[BaseAddr], Set.empty[BaseAddr], Set.empty[BaseAddr], Set.empty[BaseAddr])) { (acc, addr) =>
      val value1 = b1.getOrElse(addr, baseLattice.bottom)
      val value2 = b2.getOrElse(addr, baseLattice.bottom)
      if (value1 == value2) {
        (acc._1 + addr, acc._2, acc._3, acc._4)
      } else if (baseLattice.subsumes(value1, value2)) {
        (acc._1, acc._2 + addr, acc._3, acc._4)
      } else if (baseLattice.subsumes(value2, value1)) {
        (acc._1, acc._2, acc._3 + addr, acc._4)
      } else { // neither value is more precise than the other
        (acc._1, acc._2, acc._3, acc._4 + addr)
      }
    }
  }

  /**
   * Given an analysis (that terminated), extract its "base store": a mapping from base addresses to base values That is, convert the resulting store
   * into one within the (context-insensitive) base domain
   * @param analysis
   *   the analysis from which the results need to be extracted
   * @return
   *   a store in the base domain
   */
  protected def extract(analysis: Analysis): BaseStore =
    analysis.store
      .groupBy(p => convertAddr(analysis)(p._1))
      .view
      .filterKeys(!_.isInstanceOf[PrmAddr])
      .mapValues(m => analysis.lattice.join(m.values))
      .mapValues(convertValue(analysis))
      .toMap

  /**
   * Given a concrete interpreter (that terminated), extract its "base store": a mapping from base addresses to base values That is, convert the
   * resulting store into one within the (context-insensitive) base domain
   * @param interpreter
   *   the concrete interpreter from which the results need to be extracted
   * @return
   *   a store in the base domain
   */
  protected def extract(interpreter: SchemeInterpreter): BaseStore =
    interpreter.store.view
      .mapValues(convertConcreteValue)
      .groupBy(p => convertConcreteAddr(p._1))
      .view
      .filterKeys(!_.isInstanceOf[PrmAddr])
      .mapValues(m => baseLattice.join(m.map(_._2)))
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
    ): Option[BaseStore] =
    try {
      val anl = analysis(program)
      println(s"... analysing $path using $name ...")
      anl.analyzeWithTimeout(timeout)
      if (anl.finished) {
        Some(extract(anl))
      } else {
        None
      }
    } catch {
      case e: Exception =>
        println(s"Analyzer failed with exception $e")
        None
      case e: VirtualMachineError =>
        System.gc()
        println(s"Analyzer failed with error $e")
        None
    }

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
    ): Option[BaseStore] = {
    print(s"Running concrete interpreter on $path ($times times)")
    val preluded = SchemePrelude.addPrelude(program)
    val undefined = SchemeParser.undefine(List(preluded))
    var baseStore: BaseStore = Map.empty
    try {
      for (_ <- 1 to times) {
        print(".")
        val interpreter = new SchemeInterpreter((i, v) => (), io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")))
        interpreter.run(undefined, timeout)
        baseStore = join(baseStore, extract(interpreter))
      }
      println()
      Some(baseStore)
    } catch {
      case e: Exception =>
        println(s"Concrete interpreter failed with $e")
        None
      case e: VirtualMachineError =>
        System.gc()
        println(s"Concrete interpreter failed with $e")
        None
    }
  }

  /**
   * Specify what needs to be done for a given benchmark program
   * @param path
   *   the path to the benchmark program
   * @param program
   *   the Scheme expression of the entire program
   */
  protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit

  /**
   * Run a benchmark
   * @param benchmark
   *   the benchmark program to run
   */
  def runBenchmark(benchmark: Benchmark) = {
    val txt = Reader.loadFile(benchmark)
    val prg = SchemeParser.parse(txt)
    forBenchmark(benchmark, prg)
  }
}

package maf.modular.incremental.scheme.lattice

import maf.core.{Address, Error, LatticeTopUndefined, MayFail}
import maf.language.CScheme.TID
import maf.language.scheme.lattices._
import maf.lattice.interfaces._
import maf.modular.incremental.scheme.modconc.IncrementalSchemeLattice
import maf.util.{Monoid, MonoidInstances, SmartHash}

class IncrementalModularSchemeLattice[
    A <: Address,
    S: StringLattice,
    B: BoolLattice,
    I: IntLattice,
    R: RealLattice,
    C: CharLattice,
    Sym: SymbolLattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym] {

  type Sources = Set[Address]

  /**
   * *
   * A value that keeps track of the store addresses it depends upon.
   * @param vs      The list of abstract values.
   * @param sources The addresses in the store where the value originated.
   */
  case class AnnotatedElements(vs: List[Value], sources: Sources) extends SmartHash {
    override def toString: String =
      if (vs.isEmpty) {
        "⊥"
      } else if (vs.tail.isEmpty) {
        vs.head.toString
      } else {
        vs.map(_.toString).sorted.mkString("{", ",", "}")
      }
    def foldMapL[X](f: Value => X)(implicit monoid: Monoid[X]): X = {
      vs.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))
    }
    def toL(): L = Elements(vs)
    def joinSourcesAndGet(other: AnnotatedElements): Sources = sources.union(other.sources)
  }
  type AL = AnnotatedElements
  object AnnotatedElement extends Serializable {
    def apply(v: Value): AL = AnnotatedElements(List(v), Set())
    def apply(v: Value, sources: Sources): AL = AnnotatedElements(List(v), sources)
  }

  import maf.util.MonoidInstances._
  implicit val alMonoid: Monoid[AL] = new Monoid[AL] {
    private def insert(vs: List[Value], v: Value): List[Value] = vs match {
      case scala.Nil                     => List(v)
      case v0 :: _ if v.ord < v0.ord     => v :: vs
      case v0 :: rest if v.ord == v0.ord => Value.join(v, v0) :: rest
      case v0 :: rest                    => v0 :: insert(rest, v)
    }
    def append(x: AL, y: => AL): AL = (x, y) match {
      case (AnnotatedElements(as, asrc), AnnotatedElements(bs, bsrc)) => {
        val value = bs.foldLeft(as)(insert)
        val sources = asrc.union(bsrc)
        AnnotatedElements(value, sources)
      }
    }
    def zero: AL = AnnotatedElements(scala.Nil, Set())
  }
  implicit val alMFMonoid: Monoid[MayFail[AL, Error]] = MonoidInstances.mayFail[AL]

  val incrementalSchemeLattice: IncrementalSchemeLattice[AL, A] = new IncrementalSchemeLattice[AL, A] {
    private def annotate(als: Elements, sources: Sources): AL = AnnotatedElements(als.vs, sources)

    def show(x: AL): String = x.toString /* TODO[easy]: implement better */
    def isTrue(x: AL): Boolean = x.foldMapL(Value.isTrue(_))(boolOrMonoid)
    def isFalse(x: AL): Boolean = x.foldMapL(Value.isFalse(_))(boolOrMonoid)
    def op(op: SchemeOp)(args: List[AL]): MayFail[AL, Error] = {
      val sources = args.flatMap(_.sources).toSet // Combine all sources.
      def fold(argsToProcess: List[L], argsvRev: List[Value]): MayFail[AL, Error] = argsToProcess match {
        case arg :: args =>
          arg.foldMapL(argv => fold(args, argv :: argsvRev))
        case List() =>
          val argsv = argsvRev.reverse
          op match {
            // TODO check operations on vectors (are sources correctly combined or don't need all sources to be combined?)
            case SchemeOp.Car => Value.car(argsv.head).map(annotate(_, sources))
            case SchemeOp.Cdr => Value.cdr(argsv.head).map(annotate(_, sources))
            case SchemeOp.VectorRef =>
              Value.vectorRef(argsv.head, argsv(1)).map(annotate(_, sources))
            case SchemeOp.VectorSet =>
              Value.vectorSet(argsv.head, argsv(1), args(2).toL()).map(annotate(_, sources))
            case _ => Value.op(op)(argsv).map(x => AnnotatedElement(x, sources))
          }
      }
      op.checkArity(args)
      op match {
        case SchemeOp.MakeVector =>
          /* Treated as a special case because args(1) can be bottom (this would be a valid use of MakeVector) */
          args.head.foldMapL(arg0 => Value.vector(arg0, args(1).toL()).map(v => AnnotatedElement(v, args.head.sources.union(args(1).sources))))
        case _ => fold(args.map(_.toL()), List())
      }
    }
    def join(x: AL, y: => AL): AL = Monoid[AL].append(x, y)
    def subsumes(x: AL, y: => AL): Boolean =
      y.foldMapL(y =>
        /* For every element in y, there exists an element of x that subsumes it */
        x.foldMapL(x => Value.subsumes(x, y))(boolOrMonoid)
      )(boolAndMonoid)
    def top: AL = throw LatticeTopUndefined

    def getClosures(x: AL): Set[Closure] = x.foldMapL(x => Value.getClosures(x))(setMonoid)
    def getContinuations(x: AL): Set[K] = x.foldMapL(x => Value.getContinuations(x))(setMonoid)
    def getPrimitives(x: AL): Set[String] = x.foldMapL(x => Value.getPrimitives(x))(setMonoid)
    def getPointerAddresses(x: AL): Set[A] = x.foldMapL(x => Value.getPointerAddresses(x))(setMonoid)
    def getThreads(x: AL): Set[TID] = x.foldMapL(Value.getThreads)(setMonoid)
    def acquire(lock: AL, tid: TID): MayFail[AL, Error] =
      lock.foldMapL(l => Value.acquire(l, tid).map(annotate(_, lock.sources)))
    def release(lock: AL, tid: TID): MayFail[AL, Error] =
      lock.foldMapL(l => Value.release(l, tid).map(annotate(_, lock.sources)))

    def bottom: AL = AnnotatedElements(List.empty, Set())

    def number(x: BigInt): AL = AnnotatedElement(Value.number(x))

    def numTop: AL = AnnotatedElement(Int(IntLattice[I].top))
    def charTop: AL = AnnotatedElement(Char(CharLattice[C].top))
    def realTop: AL = AnnotatedElement(Real(RealLattice[R].top))
    def stringTop: AL = AnnotatedElement(Str(StringLattice[S].top))
    def symbolTop: AL = AnnotatedElement(Symbol(SymbolLattice[Sym].top))
    def real(x: Double): AL = AnnotatedElement(Value.real(x))
    def string(x: String): AL = AnnotatedElement(Value.string(x))
    def char(x: scala.Char): AL = AnnotatedElement(Value.char(x))
    def bool(x: Boolean): AL = AnnotatedElement(Value.bool(x))
    def primitive(x: String): AL = AnnotatedElement(Value.primitive(x))
    def closure(x: Closure): AL = AnnotatedElement(Value.closure(x))
    def cont(x: K): AL = AnnotatedElement(Value.cont(x))
    def symbol(x: String): AL = AnnotatedElement(Value.symbol(x))
    def cons(car: AL, cdr: AL): AL = AnnotatedElement(Value.cons(car.toL(), cdr.toL()), car.joinSourcesAndGet(cdr))
    def pointer(a: A): AL = AnnotatedElement(Value.pointer(a))
    def thread(tid: TID): AL = AnnotatedElement(Value.thread(tid))
    def lock(threads: Set[TID]): AL = AnnotatedElement(Value.lock(threads))
    def nil: AL = AnnotatedElement(Value.nil)
    def void: AL = AnnotatedElement(Value.void)
    def eql[B2: BoolLattice](x: AL, y: AL): B2 = ??? // TODO[medium] implement

    override def addAddresses(v: AL, addresses: Sources): AL = AnnotatedElements(v.vs, v.sources.union(addresses))
    override def clean(v: AL): AL = v.copy(sources = Set())
  }

  object AL {
    implicit val lattice: IncrementalSchemeLattice[AL, A] = incrementalSchemeLattice
  }
}

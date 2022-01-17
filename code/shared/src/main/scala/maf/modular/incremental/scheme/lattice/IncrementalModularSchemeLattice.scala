package maf.modular.incremental.scheme.lattice

import maf.core.*
import maf.language.CScheme.TID
import maf.language.ContractScheme.ContractValues.*
import maf.language.scheme.lattices.*
import maf.lattice.interfaces.*
import maf.modular.scheme.PtrAddr
import maf.util.MonoidInstances.setMonoid
import maf.util.benchmarks.Table
import maf.util.*

/** A modular Scheme lattice that also provides operations on address-annotated values. */
class IncrementalModularSchemeLattice[
    A <: Address,
    S: StringLattice,
    B: BoolLattice,
    I: IntLattice,
    R: RealLattice,
    C: CharLattice,
    Sym: SymbolLattice]
    extends ModularSchemeLattice[A, S, B, I, R, C, Sym]:

    type Sources = Set[A]

    /**
     * * A value that keeps track of the store addresses it depends upon.
     * @param values
     *   The list of abstract values.
     * @param sources
     *   The addresses in the store where the value originated.
     */
    case class AnnotatedElements(values: List[Value], sources: Sources) extends SmartHash:
        override def toString: String = toL().toString
        def foldMapL[X](f: Value => X)(implicit monoid: Monoid[X]): X =
          values.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))

        /** Convert to a non-annotated value, for use with the non-annotated lattice implementation. */
        def toL(): L = Elements(values)
        def joinedSources(other: AnnotatedElements): Sources = sources.union(other.sources) //TODO use a set monoid for this?
    type AL = AnnotatedElements
    object AnnotatedElement extends Serializable:
        def apply(v: Value): AL = AnnotatedElements(List(v), Set())
        def apply(v: Value, sources: Sources): AL = AnnotatedElements(List(v), sources)

    import maf.util.MonoidInstances._

    /** Provides the monoid implementation for annotated elements AL. */
    implicit val alMonoid: Monoid[AL] = new Monoid[AL] {
      private def insert(vs: List[Value], v: Value): List[Value] = vs match
          case scala.Nil                     => List(v)
          case v0 :: _ if v.ord < v0.ord     => v :: vs
          case v0 :: rest if v.ord == v0.ord => Value.join(v, v0) :: rest
          case v0 :: rest                    => v0 :: insert(rest, v)
      def append(xs: AL, ys: => AL): AL = AnnotatedElements(ys.values.foldLeft(xs.values)(insert), xs.sources.union(ys.sources))
      def zero: AL = AnnotatedElements(scala.Nil, Set())
    }
    implicit val alMFMonoid: Monoid[MayFail[AL, Error]] = MonoidInstances.mayFail[AL]

    /** The actual lattice implementation for AL and A. */
    val incrementalSchemeLattice: IncrementalSchemeLattice[AL, A] = new IncrementalSchemeLattice[AL, A] {

      /** Converts an elements from the non-annotated lattice to the annotated lattice. */
      private def annotate(als: Elements, sources: Sources): AL = AnnotatedElements(als.vs, sources)

      def show(x: AL): String = x.toString /* TODO[easy]: implement better */
      def refs(x: AL): Set[Address] = x.foldMapL(Value.refs(_))(setMonoid)
      def isTrue(x: AL): Boolean = x.foldMapL(Value.isTrue(_))(boolOrMonoid)
      def isFalse(x: AL): Boolean = x.foldMapL(Value.isFalse(_))(boolOrMonoid)
      def isOpq(x: AL): Boolean = x.foldMapL(Value.isOpq(_))(boolOrMonoid)

      /** Apply an operation: apply the operation on the values and join the annotations. */
      def op(op: SchemeOp)(args: List[AL]): MayFail[AL, Error] =
          val sources = args.flatMap(_.sources).toSet // Combine all sources (= join).
          // Redirect to the non-annotated lattice and annotate afterwards. This allows more reuse of code.
          schemeLattice.op(op)(args.map(_.toL())).map(annotate(_, sources))
      def join(x: AL, y: => AL): AL =
          val ys =
            y // Breaks laziness: we want y to be evaluated exactly once. If not, y will be evaluates twice (once to get the value and once to get the sources).
          AnnotatedElements(schemeLattice.join(x.toL(), ys.toL()).vs, x.sources.union(ys.sources))
      // Monoid[AL].append(x, y)
      def subsumes(x: AL, y: => AL): Boolean = schemeLattice.subsumes(x.toL(), y.toL())
      def top: AL = throw LatticeTopUndefined

      def getClosures(x: AL): Set[Closure] = schemeLattice.getClosures(x.toL())
      def getContinuations(x: AL): Set[K] = schemeLattice.getContinuations(x.toL())
      def getPrimitives(x: AL): Set[String] = schemeLattice.getPrimitives(x.toL())
      def getPointerAddresses(x: AL): Set[A] = schemeLattice.getPointerAddresses(x.toL())
      def getThreads(x: AL): Set[TID] = schemeLattice.getThreads(x.toL())
      def getBlames(x: AL): Set[Blame] = schemeLattice.getBlames(x.toL())
      def getGrds(x: AL): Set[Grd[AL]] =
        schemeLattice.getGrds(x.toL()).map(_.map(annotate(_, Set()))) // TODO[medium] not sure what to pass to annotate
      def getArrs(x: AL): Set[Arr[AL]] =
        schemeLattice.getArrs(x.toL()).map(_.map(annotate(_, Set()))) // TODO[medium] not sure what to pass to annotate
      def getFlats(x: AL): Set[Flat[AL]] =
        schemeLattice.getFlats(x.toL()).map(_.map(annotate(_, Set()))) // TODO[medium] not sure what to pass to annotate
      def getGetterSetter(x: AL): Set[StructSetterGetter] =
        schemeLattice.getGetterSetter(x.toL())
      def getStructs(x: AL): Set[Struct[AL]] =
        schemeLattice.getStructs(x.toL()).map(_.map(annotate(_, Set())))
      def getStructConstructor(x: AL): Set[StructConstructor] =
        schemeLattice.getStructConstructor(x.toL())
      def getStructPredicates(x: AL): Set[StructPredicate] =
        schemeLattice.getStructPredicates(x.toL())

      def acquire(lock: AL, tid: TID): MayFail[AL, Error] = schemeLattice.acquire(lock.toL(), tid).map(annotate(_, lock.sources))
      def release(lock: AL, tid: TID): MayFail[AL, Error] = schemeLattice.release(lock.toL(), tid).map(annotate(_, lock.sources))

      def bottom: AL = AnnotatedElements(List.empty, Set())

      override def isBottom(x: AL): Boolean = removeAddresses(x) == bottom // Just check the value.

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
      def cons(car: AL, cdr: AL): AL = AnnotatedElement(Value.cons(car.toL(), cdr.toL()), car.joinedSources(cdr))
      def pointer(a: A): AL = AnnotatedElement(Value.pointer(a))
      def thread(tid: TID): AL = AnnotatedElement(Value.thread(tid))
      def lock(threads: Set[TID]): AL = AnnotatedElement(Value.lock(threads))
      def blame(blame: Blame): AL = AnnotatedElement(Value.blame(blame))
      def grd(grd: Grd[AL]): AL = AnnotatedElement(Value.grd(grd.map(_.toL())))
      def arr(arr: Arr[AL]): AL = AnnotatedElement(Value.arr(arr.map(_.toL())))
      def flat(flt: Flat[AL]): AL = AnnotatedElement(Value.flt(flt.map(_.toL())))
      def struct(struct: Struct[AL]): AL = AnnotatedElement(Value.struct(struct.map(_.toL())))
      def structConstructor(constr: StructConstructor): AL = AnnotatedElement(Value.structConstructor(constr))
      def structPredicate(pred: StructPredicate): AL = AnnotatedElement(Value.structPredicate(pred))
      def structSetterGetter(setterGetter: StructSetterGetter): AL =
        AnnotatedElement(Value.structSetterGetter(setterGetter))
      def opq(opq: Opq): AL = AnnotatedElement(Value.opq(opq))
      def nil: AL = AnnotatedElement(Value.nil)
      def void: AL = AnnotatedElement(Value.void)
      def eql[B2: BoolLattice](x: AL, y: AL): B2 = schemeLattice.eql(x.toL(), y.toL())(BoolLattice[B2])
      def eq(xs: AL, ys: AL)(cmp: MaybeEq[A]): AL = annotate(schemeLattice.eq(xs.toL(), ys.toL())(cmp), xs.joinedSources(ys))

      override def addAddresses(v: AL, addresses: Sources): AL = AnnotatedElements(v.values, v.sources.union(addresses))
      override def getAddresses(v: AL): Set[A] = v.sources
      override def removeAddresses(v: AL): AL = v.copy(sources = Set())

      /** Shows the differences between x and y (from the perspective of a modular lattice). */
      def compare(
          x: AL,
          y: AL,
          xname: String = "x",
          yname: String = "y"
        ): String =
          var table = Table.empty.withDefaultValue("")
          def insertList(name: String, values: List[Value]): Unit =
            values.foreach { v =>
              if v.ord == 9 then // Pointers.
                  val ptrs = v
                    .asInstanceOf[Pointer]
                    .ptrs
                    .map(addr => {
                      val poi = addr.asInstanceOf[PtrAddr[_]]
                      s"${poi.exp.idn.pos.toString} [${poi.ctx}]"
                    })
                    .toList
                    .sorted
                    .mkString("Pointer(", ", ", ")")
                  table = table.add(v.typeName, name, ptrs)
              else table = table.add(v.typeName, name, v.toString)
            }
          insertList(xname, x.values)
          insertList(yname, y.values)
          (x.values.diff(y.values) ++ y.values.diff(x.values)).map(_.typeName).foreach { typeName =>
            table = table.add(typeName, "diff", "!")
          }
          table.prettyString(columns = List("diff", xname, yname))
    }

    object AL:
        implicit val lattice: IncrementalSchemeLattice[AL, A] = incrementalSchemeLattice

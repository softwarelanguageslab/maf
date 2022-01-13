package maf.language.scheme.lattices

import maf.core._
import maf.language.ContractScheme.ContractValues._
import maf.language.CScheme.TID
import maf.language.scheme.primitives._
import maf.language.scheme._
import maf.lattice.interfaces.BoolLattice
import maf.util._

class TypeSchemeLattice[A <: Address]:

    case class L(
        str: Boolean = false,
        bool: Boolean = false,
        num: Boolean = false,
        char: Boolean = false,
        sym: Boolean = false,
        nil: Boolean = false,
        inputPort: Boolean = false,
        outputPort: Boolean = false,
        prims: Set[String] = Set.empty,
        clos: Set[(SchemeLambdaExp, schemeLattice.Env)] = Set.empty,
        ptrs: Set[A] = Set.empty,
        consCells: (L, L) = (Inject.bottom, Inject.bottom),
        arr: Boolean = false,
        grd: Boolean = false,
        flt: Boolean = false,
        opq: Boolean = false,
        struct: Boolean = false,
        setterGetter: Boolean = false,
        constructor: Boolean = false,
        structPredicate: Boolean = false)
        extends SmartHash:
        def isBottom: Boolean =
          !str && !bool && !num && !char && !sym && !nil && prims.isEmpty && clos.isEmpty && consCells._1.isBottom && consCells._2.isBottom && !arr && !grd && !flt && !opq
    object Inject:
        val bottom: L = L()
        val str: L = L(str = true)
        val bool: L = L(bool = true)
        val num: L = L(num = true)
        val char: L = L(char = true)
        val sym: L = L(sym = true)
        val nil: L = L(nil = true)
        val inputPort: L = L(inputPort = true)
        val outputPort: L = L(outputPort = true)
        def prim(p: String): L = L(prims = Set(p))
        def pointer(a: A): L = L(ptrs = Set(a))
        def clo(clo: schemeLattice.Closure): L = L(clos = Set(clo))
        def cons(car: L, cdr: L): L = L(consCells = (car, cdr))
        def arr: L = L(arr = true)
        def grd: L = L(grd = true)
        def flt: L = L(flt = true)
        def opq: L = L(opq = true)
        def struct: L = L(struct = true)
        def setterGetter: L = L(setterGetter = true)
        def structConstructor: L = L(constructor = true)
        def structPredicate: L = L(structPredicate = true)

    def check(b: Boolean, v: L)(name: String, args: List[L]): MayFail[L, Error] =
      if b then { MayFail.success(v) }
      else { MayFail.failure(OperatorNotApplicable(name, args)) }

    val schemeLattice: SchemeLattice[L, A] = new SchemeLattice[L, A] {
      def show(x: L): String = s"$x"
      def isTrue(x: L): Boolean = true // only "false" is not true, but we only have Bool represented
      def isFalse(x: L): Boolean = x.bool
      def isOpq(x: L): Boolean = x.opq

      def refs(x: L): Set[Address] =
        x.ptrs ++ refs(x.consCells._1) ++ refs(x.consCells._2) ++ x.clos.flatMap(_._2.addrs)
      def op(op: SchemeOp)(args: List[L]): MayFail[L, Error] =
          import SchemeOp._
          op.checkArity(args)
          if args.exists(_.isBottom) then { MayFail.success(bottom) }
          else
              op match
                  case Car        => MayFail.success(args(0).consCells._1)
                  case Cdr        => MayFail.success(args(0).consCells._2)
                  case MakeVector => throw new Exception("NYI: vectors in type lattice")
                  case VectorRef  => throw new Exception("NYI: vectors in type lattice")
                  case VectorSet  => throw new Exception("NYI: vectors in type lattice")
                  case IsNull | IsCons | IsPointer | IsChar | IsSymbol | IsInteger | IsString | IsReal | IsBoolean | IsVector | IsThread | IsLock |
                      IsProcedure | IsInputPort | IsOutputPort | Not | IsTrue | IsFalse =>
                    // Any -> Bool
                    MayFail.success(Inject.bool)
                  case Ceiling | Floor | Round | Log | Random | Sin | ASin | Cos | ACos | Tan | ATan | Sqrt | ExactToInexact | InexactToExact =>
                    // Num -> Num
                    check(args(0).num, Inject.num)(op.name, args)
                  case VectorLength =>
                    // Vector -> Num
                    throw new Exception("NYI: vectors in type lattice")
                  case StringLength =>
                    // String -> Num
                    check(args(0).str, Inject.str)(op.name, args)
                  case StringSet =>
                    check(args(0).str, Inject.str)(op.name, args)
                  case NumberToString =>
                    // Number -> String
                    check(args(0).num, Inject.str)(op.name, args)
                  case SymbolToString =>
                    // Symbol -> String
                    check(args(0).sym, Inject.str)(op.name, args)
                  case StringToSymbol =>
                    // String -> Symbol
                    check(args(0).str, Inject.sym)(op.name, args)
                  case StringToNumber =>
                    // String -> Num
                    check(args(0).str, Inject.num)(op.name, args)
                  case IntegerToCharacter =>
                    // Num -> Char
                    check(args(0).num, Inject.char)(op.name, args)
                  case CharacterToInteger =>
                    // Char -> Num
                    check(args(0).char, Inject.num)(op.name, args)
                  case CharacterToString =>
                    // Char -> String
                    check(args(0).char, Inject.str)(op.name, args)
                  case CharacterDowncase | CharacterUpcase =>
                    // Char -> Char
                    check(args(0).char, Inject.char)(op.name, args)
                  case CharacterIsLower | CharacterIsUpper =>
                    // Char -> Bool
                    check(args(0).char, Inject.bool)(op.name, args)
                  case MakeInputPort =>
                    // String -> InputPort
                    check(args(0).str, Inject.inputPort)(op.name, args)
                  case MakeOutputPort =>
                    // String -> OutputPort
                    check(args(0).str, Inject.outputPort)(op.name, args)
                  case Plus | Minus | Times | Quotient | Div | Expt | Modulo | Remainder =>
                    // Num -> Num -> Num
                    check(args(0).num && args(1).num, Inject.num)(op.name, args)
                  case Lt | NumEq =>
                    // Num -> Num -> Bool
                    check(args(0).num && args(1).num, Inject.num)(op.name, args)
                  case StringAppend =>
                    // Str -> Str -> Str
                    check(args(0).str && args(1).str, Inject.str)(op.name, args)
                  case StringRef =>
                    // Str -> Num -> Char
                    check(args(0).str && args(1).num, Inject.char)(op.name, args)
                  case StringLt =>
                    // Str -> Str -> Bool
                    check(args(0).str && args(1).str, Inject.bool)(op.name, args)
                  case CharacterEq | CharacterLt | CharacterEqCI | CharacterLtCI =>
                    // Char -> Char -> Bool
                    check(args(0).char && args(1).char, Inject.bool)(op.name, args)
                  case MakeString =>
                    // Int -> Char -> Bool
                    check(args(0).num && args(1).char, Inject.str)(op.name, args)
                  case Substring =>
                    // Str -> Int -> Int -> Str
                    check(args(0).str && args(1).num && args(2).num, Inject.str)(op.name, args)
      def join(x: L, y: => L): L =
        L(
          str = x.str || y.str,
          bool = x.bool || y.bool,
          num = x.num || y.num,
          char = x.char || y.char,
          sym = x.sym || y.sym,
          nil = x.nil || y.nil,
          inputPort = x.inputPort || y.inputPort,
          outputPort = x.outputPort || y.outputPort,
          prims = x.prims.union(y.prims),
          clos = x.clos.union(y.clos),
          ptrs = x.ptrs.union(y.ptrs),
          consCells = (join(x.consCells._1, y.consCells._1), join(x.consCells._2, y.consCells._2)),
          arr = x.arr || y.arr,
          grd = x.grd || y.grd,
          flt = x.flt || y.flt
        )
      def subsumes(x: L, y: => L): Boolean =
        (if x.str then y.str else true) &&
          (if x.bool then y.bool else true) &&
          (if x.num then y.num else true) &&
          (if x.char then y.char else true) &&
          (if x.sym then y.sym else true) &&
          (if x.nil then y.nil else true) &&
          (if x.inputPort then y.inputPort else true) &&
          (if x.outputPort then y.outputPort else true) &&
          y.prims.subsetOf(x.prims) &&
          y.clos.subsetOf(y.clos) &&
          subsumes(x.consCells._1, y.consCells._1) &&
          subsumes(x.consCells._1, y.consCells._2) &&
          (if x.arr then y.arr else true) &&
          (if x.grd then y.grd else true) &&
          (if x.flt then y.flt else true)

      def top: L = ???
      def getClosures(x: L): Set[Closure] = x.clos
      def getPrimitives(x: L): Set[String] = x.prims
      def getPointerAddresses(x: L): Set[A] = Set()
      def getThreads(x: L): Set[TID] = throw new Exception("Not supported.")
      def getContinuations(x: L): Set[K] = ???
      def getBlames(x: L): Set[Blame] = throw new Exception("Not supported")
      def getGrds(x: L): Set[Grd[L]] = Set()
      def getArrs(x: L): Set[Arr[L]] = Set()
      def getFlats(x: L): Set[Flat[L]] = Set()
      def getStructs(x: L): Set[Struct[L]] = Set()
      def getGetterSetter(x: L): Set[StructSetterGetter] = Set()
      def getStructConstructor(x: L): Set[StructConstructor] = Set()
      def getStructPredicates(x: L): Set[StructPredicate] = Set()

      def bottom: L = Inject.bottom

      def number(x: BigInt): L = Inject.num

      def numTop: L = Inject.num
      def charTop: L = Inject.char
      def stringTop: L = Inject.str
      def realTop: L = Inject.num
      def symbolTop: L = Inject.sym
      def real(x: Double): L = Inject.num
      def string(x: String): L = Inject.str
      def bool(x: Boolean): L = Inject.bool
      def char(x: scala.Char): L = Inject.char
      def primitive(x: String): L = Inject.prim(x)
      def closure(x: schemeLattice.Closure): L = Inject.clo(x)
      def symbol(x: String): L = Inject.sym
      def nil: L = Inject.nil
      def cons(car: L, cdr: L): L = Inject.cons(car, cdr)
      def pointer(a: A): L = Inject.pointer(a)
      def eql[B: BoolLattice](x: L, y: L): B = BoolLattice[B].top /* could be refined in some cases */
      def thread(tid: TID): L = ???
      def cont(k: K): L = ???
      def lock(threads: Set[TID]) = ???
      def blame(blame: Blame): L = throw new Exception("Not supported")
      def arr(arr: Arr[L]): L = Inject.arr
      def grd(grd: Grd[L]): L = Inject.grd
      def flat(flt: Flat[L]): L = Inject.flt
      def struct(struct: Struct[L]): L = Inject.struct
      def structSetterGetter(setterGetter: StructSetterGetter): L = Inject.setterGetter
      def structConstructor(constr: StructConstructor): L = Inject.structConstructor
      def structPredicate(pred: StructPredicate): L = Inject.structPredicate
      def opq(opq: Opq): L = Inject.opq
      def void: L = ???
      def acquire(lock: L, caller: TID): MayFail[L, Error] = ???
      def release(lock: L, caller: TID): MayFail[L, Error] = ???
      def eq(x: L, y: L)(cmp: MaybeEq[A]): L = Inject.bool
    }

    object L:
        implicit val lattice: SchemeLattice[L, A] = schemeLattice

    object Primitives extends SchemeLatticePrimitives[L, A]:
        override def allPrimitives = super.allPrimitives ++ ofList(
          List(
            `abs`,
            // `assoc`, // TODO
            // `assq`, // TODO
            // `assv`, // TODO
            `display`,
            `equal?`,
            `eqv?`,
            `even?`,
            `gcd`,
            `lcm`,
            `length`,
            // `list-ref`, // TODO
            // `list->vector`, // TODO? or not
            // `list-tail`, // TODO
            `list?`,
            // `member`, // TODO
            // `memq`, // TODO
            // `memv`, // TODO
            `negative?`,
            `newline`,
            `not`,
            `odd?`,
            `positive?`,
            `zero?`,
            `<=`,
            `>`,
            `>=`
            // TODO: other cxr
            // `vector->list // TODO
            // We decided not to implement some primitives as they can't be properly supported in the framework: reverse, map, for-each, apply
          )
        )

        // shorthand (after instantiating V and A)
        class SimplePrim(val name: String, ret: L) extends SchemePrimitive[L, A]:
            def call[M[_]: PrimM](fexp: SchemeExp, args: List[L]): M[L] =
              PrimM[M].unit(ret)
        object `abs` extends SimplePrim("abs", Inject.num)
        object `display` extends SimplePrim("display", Inject.str) // undefined behavior in R5RS
        object `equal?` extends SimplePrim("equal?", Inject.bool)
        object `eqv?` extends SimplePrim("eqv?", Inject.bool)
        object `even?` extends SimplePrim("even?", Inject.bool)
        object `gcd` extends SimplePrim("gcd", Inject.num)
        object `lcm` extends SimplePrim("lcm", Inject.num)
        object `length` extends SimplePrim("length", Inject.num)
        object `list?` extends SimplePrim("list?", Inject.bool)
        object `negative?` extends SimplePrim("negative?", Inject.bool)
        object `newline` extends SimplePrim("newline", Inject.bool)
        object `not` extends SimplePrim("not", Inject.bool)
        object `odd?` extends SimplePrim("odd?", Inject.bool)
        object `positive?` extends SimplePrim("positive?", Inject.bool)
        object `zero?` extends SimplePrim("zero?", Inject.bool)
        object `<=` extends SimplePrim("<=", Inject.bool)
        object `>` extends SimplePrim(">", Inject.bool)
        object `>=` extends SimplePrim(">=", Inject.bool)

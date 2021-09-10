package maf.language.scheme.primitives

import maf.core._
import maf.language.scheme._
import maf.language.scheme.lattices.{SchemeLattice, SchemeOp}

class SchemeLatticePrimitives[V, A <: Address](implicit override val schemeLattice: SchemeLattice[V, A]) extends SchemePrimitives[V, A]:

    val lat: SchemeLattice[V, A] = schemeLattice
    val unspecified = lat.bool(false) /* TODO: introduce an "unspecified" value */

    import lat._
    import Monad._
    import MonadJoin._

    // shorthand (after instantiating V and A)
    type PrimM[M[_]] = SchemePrimM[M, A, V]
    object PrimM:
        def apply[M[_]: PrimM]: PrimM[M] = implicitly

    implicit def fromMF[M[_]: PrimM, X: Lattice](mf: MayFail[X, Error]): M[X] = mf match
        case MayFailSuccess(a)    => PrimM[M].inject(a)
        case MayFailError(errs)   => PrimM[M].mjoin(errs.map(err => PrimM[M].fail(err): M[X]))
        case MayFailBoth(a, errs) => PrimM[M].mjoin(PrimM[M].inject(a), PrimM[M].mjoin(errs.map(err => PrimM[M].fail(err): M[X])))

    def unaryOp[M[_]: PrimM](op: SchemeOp.SchemeOp1)(x: V): M[V] = lat.op(op)(List(x))
    def binaryOp[M[_]: PrimM](op: SchemeOp.SchemeOp2)(x: V, y: V): M[V] = lat.op(op)(List(x, y))
    def ternaryOp[M[_]: PrimM](op: SchemeOp.SchemeOp3)(x: V, y: V, z: V): M[V] = lat.op(op)(List(x, y, z))

    /* Simpler names for frequently used lattice operations. */
    def isInteger[M[_]: PrimM](v: V): M[V] = unaryOp(SchemeOp.IsInteger)(v)
    def isPointer[M[_]: PrimM](v: V): M[V] = unaryOp(SchemeOp.IsPointer)(v)
    def isVector[M[_]: PrimM](v: V): M[V] = unaryOp(SchemeOp.IsVector)(v)
    def isLock[M[_]: PrimM](v: V): M[V] = unaryOp(SchemeOp.IsLock)(v)
    def vectorLength[M[_]: PrimM](v: V): M[V] = unaryOp(SchemeOp.VectorLength)(v)
    def inexactToExact[M[_]: PrimM](v: V): M[V] = unaryOp(SchemeOp.InexactToExact)(v)
    def makeString[M[_]: PrimM](l: V, c: V): M[V] = binaryOp(SchemeOp.MakeString)(l, c)
    def stringAppend[M[_]: PrimM](x: V, y: V): M[V] = binaryOp(SchemeOp.StringAppend)(x, y)
    def numEq[M[_]: PrimM](x: V, y: V): M[V] = binaryOp(SchemeOp.NumEq)(x, y)
    def div[M[_]: PrimM](x: V, y: V): M[V] = binaryOp(SchemeOp.Div)(x, y)

    def ifThenElse[M[_]: PrimM](cond: M[V])(thenBranch: => M[V])(elseBranch: => M[V]): M[V] =
      PrimM[M].flatMap(cond) { condv =>
          val t = PrimM[M].flatMap(PrimM[M].guard(lat.isTrue(condv))) { _ => thenBranch }
          val f = PrimM[M].flatMap(PrimM[M].guard(lat.isFalse(condv))) { _ => elseBranch }
          PrimM[M].mjoin(t, f)
      }

    /** Dereferences a pointer x (which may point to multiple addresses) and applies a function to its value, joining everything together */
    def dereferencePointer[M[_]: PrimM, X: Lattice](x: V)(f: (A, V) => M[X]): M[X] =
      PrimM[M].deref(lat.getPointerAddresses(x))(f)

    // See comments in SchemeR5RSBenchmarks.scala for a list of all supported and unsupported primitives
    def allPrimitives: Map[String, SchemePrimitive[V, A]] =
        import PrimitiveDefs._
        ofList(
          List(
            `modulo`,
            `*`,
            SchemePlus,
            SchemeMinus,
            `/`,
            `acos`,
            `asin`,
            `atan`,
            `boolean?`,
            `true?`,
            `false?`,
            `call/cc`,
            `car`,
            `cdr`,
            `ceiling`,
            `char->integer`,
            `char->string`,
            `char-ci<?`,
            `char-ci=?`,
            `char-downcase`,
            `char-lower-case?`,
            `char-upcase`,
            `char-upper-case?`,
            `char<?`,
            `char=?`,
            `char?`,
            `cons`,
            `cos`,
            `eq?`,
            `exact->inexact`,
            `expt`,
            `floor`,
            `inexact->exact`,
            `integer->char`,
            `integer?`,
            `log`,
            `make-string`,
            `null?`,
            `number->string`,
            `number?`,
            `pair?`,
            `procedure?`,
            `quotient`,
            `real?`,
            `remainder`,
            `round`,
            `set-car!`,
            `set-cdr!`,
            `sin`,
            `sqrt`,
            `string->number`,
            `string->symbol`,
            `string-append`,
            `string-length`,
            `string-ref`,
            `string-set!`,
            `string<?`,
            `string?`,
            `substring`,
            `symbol->string`,
            `symbol?`,
            `tan`,
            `make-vector`,
            `vector`,
            `vector-length`,
            `vector-ref`,
            `vector-set!`,
            `vector?`,
            `<`,
            `=`,
            /* IO primitives */
            `input-port?`,
            `output-port?`,
            `open-input-file`,
            `open-input-string`,
            `open-output-file`,
            `close-input-port`,
            `close-output-port`,
            `current-input-port`,
            `current-output-port`,
            `read-char`,
            `peek-char`,
            `write-char`,
            `read`,
            `write`,
            `display`,
            `eof-object?`,
            /* Other primitives that are not R5RS */
            `random`,
            `error`
          ) ++ CSchemePrimitives
        )

    /** Primitives for a concurrent Scheme that are not part of R5RS. */
    def CSchemePrimitives: List[SchemePrimitive[V, A]] =
        import PrimitiveDefs._
        List(
          `new-lock`,
          `acquire`,
          `release`,
          `lock?`,
          `thread?`
        )

    abstract class SchemePrim0(val name: String) extends SchemePrimitive[V, A]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case Nil => call(fpos)
            case _   => PrimM[M].fail(PrimitiveArityError(name, 0, args.length))
        def call[M[_]: PrimM](fpos: SchemeExp): M[V]

    abstract class SchemePrim1(val name: String) extends SchemePrimitive[V, A]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case x :: Nil => call(fpos, x)
            case _        => PrimM[M].fail(PrimitiveArityError(name, 1, args.length))
        def call[M[_]: PrimM](fexp: SchemeExp, x: V): M[V]

    class SchemePrimOp1(name: String, op: SchemeOp.SchemeOp1) extends SchemePrim1(name):
        def call[M[_]: PrimM](fexp: SchemeExp, x: V): M[V] = call(x)
        def call[M[_]: PrimM](x: V): M[V] = unaryOp(op)(x)

    abstract class SchemePrim2(val name: String) extends SchemePrimitive[V, A]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case x :: y :: Nil => call(fpos, x, y)
            case _             => PrimM[M].fail(PrimitiveArityError(name, 2, args.length))
        def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V): M[V]

    class SchemePrimOp2(name: String, op: SchemeOp.SchemeOp2) extends SchemePrim2(name):
        def call[M[_]: PrimM](fex: SchemeExp, x: V, y: V): M[V] = call(x, y)
        def call[M[_]: PrimM](x: V, y: V): M[V] = binaryOp(op)(x, y)

    abstract class SchemePrim3(val name: String) extends SchemePrimitive[V, A]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case x :: y :: z :: Nil => call(fpos, x, y, z)
            case _                  => PrimM[M].fail(PrimitiveArityError(name, 3, args.length))
        def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V, z: V): M[V]

    class SchemePrimOp3(name: String, op: SchemeOp.SchemeOp3) extends SchemePrim3(name):
        def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V, z: V): M[V] = call(x, y, z)
        def call[M[_]: PrimM](x: V, y: V, z: V): M[V] = ternaryOp(op)(x, y, z)

    abstract class SchemePrimVarArg(val name: String) extends SchemePrimitive[V, A]

    object PrimitiveDefs:

        case object `<` extends SchemePrimOp2("<", SchemeOp.Lt) // TODO[easy]: < should accept any number of arguments (same for <= etc.)
        case object `acos` extends SchemePrimOp1("acos", SchemeOp.ACos)
        case object `asin` extends SchemePrimOp1("asin", SchemeOp.ASin)
        case object `atan` extends SchemePrimOp1("atan", SchemeOp.ATan)
        case object `boolean?` extends SchemePrimOp1("boolean?", SchemeOp.IsBoolean)
        case object `true?` extends SchemePrimOp1("true?", SchemeOp.IsTrue)
        case object `false?` extends SchemePrimOp1("false?", SchemeOp.IsFalse)
        case object `ceiling` extends SchemePrimOp1("ceiling", SchemeOp.Ceiling)
        case object `char->integer` extends SchemePrimOp1("char->integer", SchemeOp.CharacterToInteger)
        case object `char-ci<?` extends SchemePrimOp2("char-ci<?", SchemeOp.CharacterLtCI)
        case object `char-ci=?` extends SchemePrimOp2("char-ci=?", SchemeOp.CharacterEqCI)
        case object `char-downcase` extends SchemePrimOp1("char-downcase", SchemeOp.CharacterDowncase)
        case object `char-lower-case?` extends SchemePrimOp1("char-lower-case?", SchemeOp.CharacterIsLower)
        case object `char-upcase` extends SchemePrimOp1("char-upcase", SchemeOp.CharacterUpcase)
        case object `char-upper-case?` extends SchemePrimOp1("char-upper-case?", SchemeOp.CharacterIsUpper)
        case object `char<?` extends SchemePrimOp2("char<?", SchemeOp.CharacterLt)
        case object `char=?` extends SchemePrimOp2("char=?", SchemeOp.CharacterEq)
        case object `char?` extends SchemePrimOp1("char?", SchemeOp.IsChar)
        case object `cos` extends SchemePrimOp1("cos", SchemeOp.Cos)
        case object `eq?` extends SchemePrim2("eq?"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V) =
              for
                  aeq <- PrimM[M].addrEq // analysis determines how equality between 2 addrs is done
                  res <- PrimM[M] inject (lat.eq(x, y)(aeq))
              yield res
        case object `error` extends SchemePrim1("error"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              PrimM[M].fail(UserError(x.toString))
        case object `exact->inexact` extends SchemePrimOp1("exact->inexact", SchemeOp.ExactToInexact)
        case object `expt` extends SchemePrimOp2("expt", SchemeOp.Expt)
        case object `floor` extends SchemePrimOp1("floor", SchemeOp.Floor)
        case object `inexact->exact` extends SchemePrimOp1("inexact->exact", SchemeOp.InexactToExact)
        case object `integer->char` extends SchemePrimOp1("integer->char", SchemeOp.IntegerToCharacter)
        case object `integer?` extends SchemePrimOp1("integer?", SchemeOp.IsInteger)
        case object `log` extends SchemePrimOp1("log", SchemeOp.Log)
        case object `modulo` extends SchemePrimOp2("modulo", SchemeOp.Modulo)
        case object `null?` extends SchemePrimOp1("null?", SchemeOp.IsNull)
        case object `number?` extends SchemePrimOp1("number?", SchemeOp.IsReal)
        case object `real?` extends SchemePrimOp1("real?", SchemeOp.IsReal)
        /* No support for complex number, so number? is equivalent as real? */
        case object `procedure?` extends SchemePrimOp1("procedure?", SchemeOp.IsProcedure)
        case object `quotient` extends SchemePrimOp2("quotient", SchemeOp.Quotient)
        case object `random` extends SchemePrimOp1("random", SchemeOp.Random)
        case object `remainder` extends SchemePrimOp2("remainder", SchemeOp.Remainder)
        case object `round` extends SchemePrimOp1("round", SchemeOp.Round)
        case object `sin` extends SchemePrimOp1("sin", SchemeOp.Sin)
        case object `symbol?` extends SchemePrimOp1("symbol?", SchemeOp.IsSymbol)
        case object `tan` extends SchemePrimOp1("tan", SchemeOp.Tan)

        case object SchemePlus extends SchemePrimVarArg("+"):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = call(vs)
            def call[M[_]: PrimM](vs: List[V]): M[V] =
              vs.foldLeftM(number(0))((acc, num) => binaryOp(SchemeOp.Plus)(acc, num))

        case object SchemeMinus extends SchemePrimVarArg("-"):
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
                case Nil      => PrimM[M].fail(PrimitiveVariadicArityError("-", 1, 0))
                case x :: Nil => binaryOp(SchemeOp.Minus)(number(0), x)
                case x :: rst => SchemePlus.call(rst) >>= { (binaryOp(SchemeOp.Minus)(x, _)) }

        case object `*` extends SchemePrimVarArg("*"):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = call(vs)
            def call[M[_]: PrimM](vs: List[V]): M[V] =
              vs.foldLeftM(number(1))((acc, num) => binaryOp(SchemeOp.Times)(acc, num))

        case object `/` extends SchemePrimVarArg("/"):
            def call[M[_]: PrimM](fexp: SchemeExp, vs: List[V]): M[V] = vs match
                case Nil => PrimM[M].fail(PrimitiveVariadicArityError("/", 1, 0))
                case x :: r =>
                  for
                      multrest <- `*`.call(r)
                      r <- div(x, multrest)
                      fl <- `floor`.call(r)
                      isexact <- numEq(r, fl)
                      xisint <- isInteger(x)
                      multrestisint <- isInteger(multrest)
                      convert = and(isexact, and(xisint, multrestisint))
                      exr <- inexactToExact(r)
                      res <- ifThenElse(PrimM[M].inject(convert))(PrimM[M].inject(exr))(PrimM[M].inject(r))
                  yield res

        case object `=` extends SchemePrimVarArg("="):
            def eq[M[_]: PrimM](first: V, l: List[V]): M[V] = l match
                case Nil => PrimM[M].unit(bool(true))
                case x :: r =>
                  ifThenElse(numEq(first, x)) {
                    eq(first, r)
                  } {
                    PrimM[M].unit(bool(false))
                  }
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = vs match
                case Nil       => PrimM[M].unit(bool(true))
                case x :: rest => eq(x, rest)

        case object `sqrt` extends SchemePrim1("sqrt"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              ifThenElse(`<`.call(x, number(0))) {
                /* n < 0 */
                PrimM[M].fail(PrimitiveNotApplicable("sqrt", List(x)))
              } {
                /* n >= 0 */
                for
                    r <- unaryOp(SchemeOp.Sqrt)(x)
                    fl <- `floor`.call(r)
                    argisexact <- isInteger(x)
                    resisexact <- numEq(r, fl)
                    convert = and(argisexact, resisexact)
                    exr <- inexactToExact(r)
                    res <- ifThenElse(PrimM[M].inject(convert))(PrimM[M].inject(exr))(PrimM[M].inject(r))
                yield res
              }

        abstract class SchemePrimRefTypeCheck(name: String, check: SchemeOp.SchemeOp1) extends SchemePrim1(name):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              ifThenElse(isPointer(x)) {
                dereferencePointer(x) { (_, vlu) =>
                  unaryOp(check)(vlu)
                }
              } {
                PrimM[M].unit(bool(false))
              }

        case object `pair?` extends SchemePrimRefTypeCheck("pair?", SchemeOp.IsCons)
        case object `vector?` extends SchemePrimRefTypeCheck("vector?", SchemeOp.IsVector)
        case object `thread?` extends SchemePrimOp1("thread?", SchemeOp.IsThread)
        case object `lock?` extends SchemePrimRefTypeCheck("lock?", SchemeOp.IsLock)

        case object `string-append` extends SchemePrimVarArg("string-append"):
            private def buildString[M[_]: PrimM](args: List[V]): M[V] =
              args.foldRightM(string("")) { (x, rst) =>
                dereferencePointer(x)((_, str) => stringAppend(str, rst))
              }
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] =
              for
                  str <- buildString(args)
                  adr <- PrimM[M].allocVal(fpos, str)
              yield lat.pointer(adr)

        case object `make-string` extends SchemePrimVarArg("make-string"):
            private def mkString[M[_]: PrimM](fpos: SchemeExp, length: V, char: V): M[V] =
              for
                  str <- makeString(length, char)
                  adr <- PrimM[M].allocVal(fpos, str)
              yield lat.pointer(adr)
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
                case length :: Nil         => mkString(fpos, length, lat.char(0.toChar))
                case length :: char :: Nil => mkString(fpos, length, char)
                case l                     => PrimM[M].fail(PrimitiveArityError(name, 1, l.size))

        case object `number->string` extends SchemePrim1("number->string"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              for
                  str <- unaryOp(SchemeOp.NumberToString)(x)
                  adr <- PrimM[M].allocVal(fpos, str)
              yield lat.pointer(adr)

        case object `string->number` extends SchemePrim1("string->number"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, str) => unaryOp(SchemeOp.StringToNumber)(str) }

        case object `string->symbol` extends SchemePrim1("string->symbol"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, str) => unaryOp(SchemeOp.StringToSymbol)(str) }

        case object `string-length` extends SchemePrim1("string-length"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, str) => unaryOp(SchemeOp.StringLength)(str) }

        case object `string-ref` extends SchemePrim2("string-ref"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, idx: V): M[V] =
              dereferencePointer(x) { (_, str) => binaryOp(SchemeOp.StringRef)(str, idx) }

        case object `string-set!` extends SchemePrim3("string-set!"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, idx: V, chr: V): M[V] =
              dereferencePointer(x) { (adr, str) =>
                for
                    updatedStr <- ternaryOp(SchemeOp.StringSet)(str, idx, chr)
                    _ <- PrimM[M].updateSto(adr, updatedStr)
                yield unspecified
              }

        case object `string<?` extends SchemePrim2("string<?"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
              dereferencePointer(x) { (_, xstr) =>
                dereferencePointer(y) { (_, ystr) =>
                  binaryOp(SchemeOp.StringLt)(xstr, ystr)
                }
              }

        case object `string?` extends SchemePrimRefTypeCheck("string?", SchemeOp.IsString)

        case object `substring` extends SchemePrim3("substring"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, start: V, end: V): M[V] =
              for
                  substr <- dereferencePointer(x) { (_, str) => ternaryOp(SchemeOp.Substring)(str, start, end) }
                  adr <- PrimM[M].allocVal(fpos, substr)
              yield lat.pointer(adr)

        case object `symbol->string` extends SchemePrim1("symbol->string"):
            def call[M[_]: PrimM](fpos: SchemeExp, sym: V): M[V] =
              for
                  str <- unaryOp(SchemeOp.SymbolToString)(sym)
                  adr <- PrimM[M].allocVal(fpos, str)
              yield lat.pointer(adr)

        case object `char->string` extends SchemePrim1("char->string"):
            def call[M[_]: PrimM](fpos: SchemeExp, chr: V): M[V] =
              for
                  str <- unaryOp(SchemeOp.CharacterToString)(chr)
                  adr <- PrimM[M].allocVal(fpos, str)
              yield lat.pointer(adr)

        case object `cons` extends SchemePrim2("cons"):
            def call[M[_]: PrimM](fpos: SchemeExp, car: V, cdr: V): M[V] =
              for adr <- PrimM[M].allocVal(fpos, lat.cons(car, cdr))
              yield lat.pointer(adr)

        case object `car` extends SchemePrim1("car"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x)((_, cons) => lat.car(cons))
        case object `cdr` extends SchemePrim1("cdr"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x)((_, cons) => lat.cdr(cons))

        case object `set-car!` extends SchemePrim2("set-car!"):
            def call[M[_]: PrimM](fpos: SchemeExp, cell: V, value: V): M[V] =
              dereferencePointer(cell) { (addr, cons) =>
                for
                    cdr <- fromMF(lat.cdr(cons))
                    _ <- PrimM[M].updateSto(addr, lat.cons(value, cdr))
                yield unspecified
              }

        case object `set-cdr!` extends SchemePrim2("set-cdr!"):
            def call[M[_]: PrimM](fpos: SchemeExp, cell: V, value: V): M[V] =
              dereferencePointer(cell) { (addr, cons) =>
                for
                    car <- fromMF(lat.car(cons))
                    _ <- PrimM[M].updateSto(addr, lat.cons(car, value))
                yield unspecified
              }

        case object `make-vector` extends SchemePrimVarArg("make-vector"):
            def createVec[M[_]: PrimM](fpos: SchemeExp, size: V, init: V): M[V] =
              for
                  vec <- fromMF(lat.vector(size, init))
                  adr <- PrimM[M].allocVal(fpos, vec)
              yield lat.pointer(adr)
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
                case size :: Nil         => createVec(fpos, size, unspecified)
                case size :: init :: Nil => createVec(fpos, size, init)
                case l                   => PrimM[M].fail(PrimitiveVariadicArityError(name, 1, l.size))

        case object `vector` extends SchemePrimVarArg("vector"):
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] =
              for
                  emptyVec <- fromMF(lat.vector(lat.number(args.size), bottom))
                  filledVec <- args.zipWithIndex.foldLeftM[M, V](emptyVec) { case (acc, (arg, idx)) =>
                    vectorSet(acc, lat.number(idx), arg)
                  }
                  adr <- PrimM[M].allocVal(fpos, filledVec)
              yield lat.pointer(adr)

        case object `vector-length` extends SchemePrim1("vector-length"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, vec) => vectorLength(vec) }

        case object `vector-ref` extends SchemePrim2("vector-ref"):
            def call[M[_]: PrimM](fpos: SchemeExp, v: V, idx: V): M[V] =
              dereferencePointer(v) { (_, vec) => lat.vectorRef(vec, idx) }

        case object `vector-set!` extends SchemePrim3("vector-set!"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, index: V, newval: V): M[V] =
              dereferencePointer(x) { (adr, vec) =>
                for
                    newvec <- fromMF(lat.vectorSet(vec, index, newval))
                    _ <- PrimM[M].updateSto(adr, newvec)
                yield unspecified
              }

        case object `call/cc` extends SchemePrim1("call/cc"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              getClosures(x).foldMapM { clo =>
                for
                    _ <- PrimM[M].guard(clo._1.check(1))
                    res <- PrimM[M].callcc(clo, fpos.idn.pos)
                yield res
              }

        case object `input-port?` extends SchemePrimOp1("input-port?", SchemeOp.IsInputPort)
        case object `output-port?` extends SchemePrimOp1("output-port?", SchemeOp.IsOutputPort)

        case object `open-input-file` extends SchemePrim1("open-input-file"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, str) =>
                ifThenElse(unaryOp(SchemeOp.IsString)(str)) {
                  for
                      // TODO: this could be cleaner by having a difference between a file input port and string input port, but this would be a bit overkill
                      portstring <- binaryOp(SchemeOp.StringAppend)(string("__file__"), str)
                      inputPort <- unaryOp(SchemeOp.MakeInputPort)(portstring)
                  yield inputPort
                } {
                  PrimM[M].fail(PrimitiveNotApplicable("open-input-file", List(x)))
                }
              }

        case object `open-input-string` extends SchemePrim1("open-input-string"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, str) =>
                ifThenElse(unaryOp(SchemeOp.IsString)(str)) {
                  unaryOp(SchemeOp.MakeInputPort)(str)
                } {
                  PrimM[M].fail(PrimitiveNotApplicable("open-input-string", List(x)))
                }
              }

        case object `open-output-file` extends SchemePrim1("open-output-file"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (_, str) =>
                ifThenElse(unaryOp(SchemeOp.IsString)(str)) {
                  for
                      // TODO: this could be cleaner by having a difference between a file input port and string input port, but this would be a bit overkill
                      portstring <- binaryOp(SchemeOp.StringAppend)(string("__file__"), str)
                      outputPort <- unaryOp(SchemeOp.MakeOutputPort)(portstring)
                  yield outputPort
                } {
                  PrimM[M].fail(PrimitiveNotApplicable("open-output-file", List(x)))
                }
              }

        case object `close-input-port` extends SchemePrim1("close-input-port"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              ifThenElse(unaryOp(SchemeOp.IsInputPort)(x)) {
                PrimM[M].unit(unspecified)
              } {
                PrimM[M].fail(PrimitiveNotApplicable("close-input-port", List(x)))
              }

        case object `close-output-port` extends SchemePrim1("close-output-port"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              ifThenElse(unaryOp(SchemeOp.IsOutputPort)(x)) {
                PrimM[M].unit(unspecified)
              } {
                PrimM[M].fail(PrimitiveNotApplicable("close-output-port", List(x)))
              }

        case object `current-input-port` extends SchemePrim0("current-input-port"):
            def call[M[_]: PrimM](fpos: SchemeExp): M[V] = unaryOp(SchemeOp.MakeInputPort)(string("__console__"))

        case object `current-output-port` extends SchemePrim0("current-output-port"):
            def call[M[_]: PrimM](fpos: SchemeExp): M[V] = unaryOp(SchemeOp.MakeOutputPort)(string("__console__"))

        class ReadOrPeekChar(name: String) extends SchemePrimVarArg(name):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = vs match
                case Nil => PrimM[M].unit(charTop)
                case inp :: Nil =>
                  ifThenElse(unaryOp(SchemeOp.IsInputPort)(inp)) {
                    PrimM[M].unit(charTop)
                  } {
                    PrimM[M].fail(PrimitiveNotApplicable(name, vs))
                  }
                case l => PrimM[M].fail(PrimitiveArityError(name, 1, l.size))

        case object `read-char` extends ReadOrPeekChar("read-char")
        case object `peek-char` extends ReadOrPeekChar("peek-char")

        case object `write-char` extends SchemePrimVarArg("write-char"):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] =
                def check(res: M[V]): M[V] =
                  ifThenElse(res) {
                    PrimM[M].unit(unspecified)
                  } {
                    PrimM[M].fail(PrimitiveNotApplicable(name, vs))
                  }
                vs match
                    case chr :: Nil => check(unaryOp(SchemeOp.IsChar)(chr))
                    case chr :: out :: Nil =>
                      val res = for
                          isChar <- unaryOp(SchemeOp.IsChar)(chr)
                          isPort <- unaryOp(SchemeOp.IsOutputPort)(out)
                      yield and(isChar, isPort)
                      check(res)
                    case l => PrimM[M].fail(PrimitiveArityError(name, 2, l.size))

        class WriteOrDisplay(name: String) extends SchemePrimVarArg(name):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = vs match
                case _ :: Nil => PrimM[M].unit(unspecified)
                case _ :: outputPort :: Nil =>
                  ifThenElse(unaryOp(SchemeOp.IsOutputPort)(outputPort)) {
                    PrimM[M].unit(unspecified)
                  } {
                    PrimM[M].fail(PrimitiveNotApplicable(name, vs))
                  }
                case l => PrimM[M].fail(PrimitiveArityError(name, 1, l.size))

        case object `display` extends WriteOrDisplay("display")
        case object `write` extends WriteOrDisplay("write")

        case object `read` extends SchemePrimVarArg("read"):
            def topValue[M[_]: PrimM](fpos: SchemeExp): M[V] =
              for
                  adr <- PrimM[M].allocPtr(fpos)
                  ptr = lat.pointer(adr)
                  vlu = lat.join(Seq(ptr, nil, numTop, realTop, charTop, symbolTop, boolTop))
                  cns = lat.cons(vlu, vlu)
                  // Note #1: creating a vector with these arguments is known to succeed
                  // Note #2: vector (and string) should use a different address than the cons-cell
                  // Note #3: need to ensure that abstract count == +inf for adr!
                  MayFailSuccess(vct) = lat.vector(numTop, vlu)
                  _ <- PrimM[M].extendSto(adr, cns)
                  _ <- PrimM[M].extendSto(adr, vct)
                  _ <- PrimM[M].extendSto(adr, stringTop)
              yield vlu
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
                case Nil => topValue(fpos)
                case inp :: Nil =>
                  ifThenElse(unaryOp(SchemeOp.IsInputPort)(inp)) {
                    topValue(fpos)
                  } {
                    PrimM[M].fail(PrimitiveNotApplicable(name, args))
                  }
                case oth => PrimM[M].fail(PrimitiveArityError(name, 1, oth.size))

        case object `eof-object?` extends SchemePrim1("eof-object?"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              /* TODO: there is no specific encoding for EOF objects, but they can only arise in scenarios where charTop is produced. So we can approximate them as follows */
              if subsumes(x, charTop) then PrimM[M].unit(boolTop)
              else PrimM[M].unit(bool(false))

        case object `new-lock` extends SchemePrim0("new-lock"):
            def call[M[_]: PrimM](fpos: SchemeExp): M[V] =
              for adr <- PrimM[M].allocVal(fpos, lat.lock(Set.empty))
              yield lat.pointer(adr)

        case object `acquire` extends SchemePrim1("acquire"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (addr, lock) =>
                for
                    thread <- PrimM[M].currentThread
                    locked <- fromMF(lat.acquire(lock, thread))
                    _ <- PrimM[M].updateSto(addr, locked)
                yield unspecified
              }

        case object `release` extends SchemePrim1("release"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
              dereferencePointer(x) { (addr, lock) =>
                for
                    thread <- PrimM[M].currentThread
                    unlocked <- fromMF(lat.release(lock, thread))
                    _ <- PrimM[M].updateSto(addr, unlocked)
                yield unspecified
              }

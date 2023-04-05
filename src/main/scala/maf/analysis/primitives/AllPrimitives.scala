package maf
package analysis.primitives

import cats.{MonadError => _, *}
import analysis.store.*
import cats.extensions.*
import maf.syntax.scheme.*
import cats.syntax.all.*
import util.*
import values.*
import values.scheme.*
import values.typeclasses.*
import maf.interpreter.*
import maf.interpreter.SimpleSchemeValue
import cats.extensions.Errors.raiseError
import maf.values.scheme.Extractor

trait SchemePrimitive[V: Lattice, Vec, Pai, A <: Address]:
    // Every primitive in Scheme has a unique name
    def name: String
    // They can be called given the calling expression and arguments using any compatible SchemePrimM monad
    def call[M[_]](
        fexp: SchemeExp,
        args: List[V]
      )(implicit m: SchemePrimM[M, V, Vec, Pai]
      ): M[V]

// Primitive-specific errors
case class PrimitiveArityError(name: String, expected: Int, got: Int) extends Error
case class PrimitiveVariadicArityError(
    name: String,
    expectedAtLeast: Int,
    got: Int)
    extends Error
case class PrimitiveNotApplicable[V](name: String, args: List[V]) extends Error
case class UserError(message: String) extends Error

class SchemeLatticePrimitives[V, Vec, Pai](
    using
    val dom: SchemeDomain[V, Vec, Pai]):

    final private val lat = dom.schemeLattice

    import dom.given
    import lat.{_, given}
    import Monad._
    import Galois.*
    import MonadJoin._
    import ConcreteSchemeValue.given

    def ofList(
        prims: List[SchemePrimitive[V, Vec, Pai, Address]]
      ): Map[String, SchemePrimitive[V, Vec, Pai, Address]] =
        prims.map(prim => (prim.name, prim)).toMap

    val unspecified: V = Galois.inject[SimpleSchemeValue, V](SchemeUnspecified)

    // shorthand (after instantiating V and A)
    type PrimM[M[_]] = SchemePrimM[M, V, Vec, Pai]
    object PrimM:
        def apply[M[_]: PrimM]: PrimM[M] = summon

    /** Dereferences a pointer x (which may point to multiple addresses) and applies a function to its value, joining everything together
      */
    def dereferencePointer[M[_]: PrimM, X: Lattice, A <: StoreAddress.Aux[V2], V2](
        x: V
      )(patt: Extractor[V, A],
        msg: String
      )(f: (A, V2) => M[X]
      ): M[X] =
        MonadJoin[M].mfoldMap(split(x)) { case patt(adr, _) =>
            PrimM[M].deref(adr)(f(adr, _))
        }

    // See comments in SchemeR5RSBenchmarks.scala for a list of all supported and unsupported primitives
    def allPrimitives: Map[String, SchemePrimitive[V, Vec, Pai, Address]] =
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
            `car`,
            `cdr`,
            `ceiling`,
            `char->integer`,
            // `char->string`,
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
            // `make-string`,
            `null?`,
            // `number->string`,
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
            // `string->number`,
            // `string->symbol`,
            // `string-append`,
            // `string-length`,
            // `string-ref`,
            // `string-set!`,
            // `string<?`,
            // `string?`,
            // `substring`,
            // `symbol->string`,
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
            // `input-port?`,
            // `output-port?`,
            // `open-input-file`,
            // `open-input-string`,
            // `open-output-file`,
            // `close-input-port`,
            // `close-output-port`,
            // `current-input-port`,
            // `current-output-port`,
            // `read-char`,
            // `peek-char`,
            // `write-char`,
            // `read`,
            // `write`,
            // `display`,
            // `eof-object?`,
            // primitives to support structs
            // StructRef,
            /* Other primitives that are not R5RS */
            `random`,
            `error`,
            `bool-top`
          ) ++ CSchemePrimitives
        )

    /** Primitives for a concurrent Scheme that are not part of R5RS. */
    def CSchemePrimitives: List[SchemePrimitive[V, Vec, Pai, Address]] =
        import PrimitiveDefs._
        List(
          // `new-lock`,
          // `acquire`,
          // `release`,
          // `lock?`,
          // `thread?`
        )

    abstract class SchemePrim0(val name: String) extends SchemePrimitive[V, Vec, Pai, Address]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case Nil => call(fpos)
            case _   => PrimM[M].raiseError(PrimitiveArityError(name, 0, args.length))
        def call[M[_]: PrimM](fpos: SchemeExp): M[V]

    abstract class SchemePrim1(val name: String) extends SchemePrimitive[V, Vec, Pai, Address]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case x :: Nil => call(fpos, x)
            case _        => PrimM[M].raiseError(PrimitiveArityError(name, 1, args.length))
        def call[M[_]: PrimM](fexp: SchemeExp, x: V): M[V]

    abstract class SchemePrim2(val name: String) extends SchemePrimitive[V, Vec, Pai, Address]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case x :: y :: Nil => call(fpos, x, y)
            case _             => PrimM[M].raiseError(PrimitiveArityError(name, 2, args.length))
        def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V): M[V]

    abstract class SchemePrim3(val name: String) extends SchemePrimitive[V, Vec, Pai, Address]:
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case x :: y :: z :: Nil => call(fpos, x, y, z)
            case _                  => PrimM[M].raiseError(PrimitiveArityError(name, 3, args.length))
        def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V, z: V): M[V]

    abstract class SchemePrimVarArg(val name: String) extends SchemePrimitive[V, Vec, Pai, Address]

    object PrimitiveDefs:
        import ConcreteSchemeValue.given
        case object `<` extends SchemePrim2("<"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] = lat.lt(x, y)
        case object `acos` extends SchemePrim1("acos"):
            def call[M[_]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.acos(x)
        case object `asin` extends SchemePrim1("asin"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.asin(x)
        case object `atan` extends SchemePrim1("atan"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.atan(x)
        case object `boolean?` extends SchemePrim1("boolean?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.isBool[V](x).pure
        case object `true?` extends SchemePrim1("true?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                Galois.inject(lat.isTrue(x)).pure
        case object `false?` extends SchemePrim1("false?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                Galois.inject(lat.isFalse(x)).pure
        case object `ceiling` extends SchemePrim1("ceiling"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.ceiling(x)
        case object `char->integer` extends SchemePrim1("char->integer"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                // TODO: add type checking
                lat.toInt(x)
        case object `char-ci<?` extends SchemePrim2("char-ci<?"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.charLtCI(x, y)
        case object `char-ci=?` extends SchemePrim2("char-ci=?"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.charEqCI(x, y)
        case object `char-downcase` extends SchemePrim1("char-downcase"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.downCase(x)
        case object `char-lower-case?` extends SchemePrim1("char-lower-case?"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.isLower(x)
        case object `char-upcase` extends SchemePrim1("char-upcase"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.upCase(x)
        case object `char-upper-case?` extends SchemePrim1("char-upper-case?"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.isUpper(x)
        case object `char<?` extends SchemePrim2("char<?"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.charLt(x, y)
        case object `char=?` extends SchemePrim2("char=?"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.charEq(x, y)
        case object `char?` extends SchemePrim1("char?"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.isChar[V](x).pure
        case object `cos` extends SchemePrim1("cos"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.cos(x)
        case object `eq?` extends SchemePrim2("eq?"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V) =
                PrimM[M].addrEq map lat.eq(x, y)
        case object `error` extends SchemePrim1("error"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
                PrimM[M].raiseError(UserError(x.toString))
        case object `exact->inexact` extends SchemePrim1("exact->inexact"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] = ??? // TODO
        case object `expt` extends SchemePrim2("expt"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.expt(x, y)
        case object `floor` extends SchemePrim1("floor"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.floor(x)
        case object `inexact->exact` extends SchemePrim1("inexact->exact"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] = ??? // TODO
        case object `integer->char` extends SchemePrim1("integer->char"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.toInt(x)
        case object `integer?` extends SchemePrim1("integer?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.isInt[V](x).pure
        case object `log` extends SchemePrim1("log"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.log(x)
        case object `modulo` extends SchemePrim2("modulo"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.modulo(x, y)
        case object `null?` extends SchemePrim1("null?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.isNull[V](x).pure
        case object `number?` extends SchemePrim1("number?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.join(lat.isReal(x), lat.isInt(x)).pure
        case object `real?` extends SchemePrim1("real?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.isReal(x).pure
        /* No support for complex number, so number? is equivalent as real? */
        case object `procedure?` extends SchemePrim1("procedure?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.join(lat.isPrim(x), lat.isClo(x)).pure
        case object `quotient` extends SchemePrim2("quotient"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.quotient(x, y)
        case object `random` extends SchemePrim1("random"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.random(x)
        case object `remainder` extends SchemePrim2("remainder"):
            def call[M[_$11]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
                lat.remainder(x, y)
        case object `round` extends SchemePrim1("round"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.round(x)
        case object `sin` extends SchemePrim1("sin"):
            def call[M[_$9]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.sin(x)
        case object `symbol?` extends SchemePrim1("symbol?"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] =
                lat.isSym(x).pure
        case object `tan` extends SchemePrim1("tan"):
            def call[M[_$8]: PrimM](fexp: SchemeExp, x: V): M[V] = lat.tan(x)
        case object `bool-top` extends SchemePrim0("bool-top"):
            override def call[M[_]: PrimM](fpos: SchemeExp): M[V] =
                lat.boolTop.pure

        case object SchemePlus extends SchemePrimVarArg("+"):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = call(vs)
            def call[M[_]: PrimM](vs: List[V]): M[V] =
                vs.foldM(Galois.inject(BigInt(0)))((acc, num) => lat.plus(acc, num))

        case object SchemeMinus extends SchemePrimVarArg("-"):
            def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
                case Nil      => PrimM[M].raiseError(PrimitiveVariadicArityError("-", 1, 0))
                case x :: Nil => lat.minus(Galois.inject(BigInt(0)), x)
                case x :: rst =>
                    SchemePlus.call(rst) >>= { (lat.minus(x, _)) }

        case object `*` extends SchemePrimVarArg("*"):
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = call(vs)
            def call[M[_]: PrimM](vs: List[V]): M[V] =
                vs.foldLeftM(Galois.inject(BigInt(1)))((acc, num) => lat.times(acc, num))

        case object `/` extends SchemePrimVarArg("/"):
            def call[M[_]: PrimM](fexp: SchemeExp, vs: List[V]): M[V] = vs match
                case Nil => PrimM[M].raiseError(PrimitiveVariadicArityError("/", 1, 0))
                case x :: r =>
                    for
                        multrest <- `*`.call(r)
                        r <- lat.div[M, V](x, multrest)
                    yield r // TODO: returns a real but could be an integer (/ 8 2) = 4.0 but should 4

        case object `=` extends SchemePrimVarArg("="):
            def eq[M[_]: PrimM](first: V, l: List[V]): M[V] = l match
                case Nil => Galois.inject(true).pure
                case x :: r =>
                    MonadJoin[M].cond(lat.eql[V](first, x)) {
                        eq(first, r)
                    } {
                        Galois.inject(false).pure
                    }
            def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = vs match
                case Nil       => Galois.inject(true).pure
                case x :: rest => eq(x, rest)

        case object `sqrt` extends SchemePrim1("sqrt"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
                lat.sqrt(
                  x
                ) // TODO: returns a real but could be an integer (for example (sqrt 16) = 4 not 4.0)

        abstract class SchemePrimRefTypeCheck[A <: StoreAddress](
            name: String,
            patt: Extractor[V, A])
            extends SchemePrim1(name):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
                MonadJoin[M].mfoldMap(split(x)) {
                    case patt(v, _) => inject(true).pure
                    case _          => inject(false).pure
                }

        case object `pair?` extends SchemePrimRefTypeCheck[PairAddress[Pai]]("pair?", lat.pairAddress)
        case object `vector?` extends SchemePrimRefTypeCheck("vector?", lat.vectorAddress)
        // case object `thread?` extends SchemePrim1("thread?")
        // case object `lock?` extends SchemePrimRefTypeCheck("lock?")

        // case object `string-append` extends SchemePrimVarArg("string-append"):
        //    private def buildString[M[_]: PrimM](args: List[V]): M[V] =
        //        args.foldRightM(Galois.inject[String, V]("")) { (x, rst) =>
        //            dereferencePointer[M, V](x)((_, str) => lat.append[M](str, rst))
        //        }
        //    def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] =
        //        for
        //            str <- buildString(args)
        //            adr <- PrimM[M].allocVal(fpos, str)
        //        yield lat.pointer(adr)

        // case object `make-string` extends SchemePrimVarArg("make-string"):
        //    private def mkString[M[_]: PrimM](
        //        fpos: SchemeExp,
        //        length: V,
        //        char: V
        //      ): M[V] =
        //        for
        //            str <- lat.makeString(length, char)
        //            adr <- PrimM[M].allocVal(fpos, str)
        //        yield lat.pointer(adr)
        //    def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
        //        case length :: Nil         => mkString(fpos, length, inject(0.toChar))
        //        case length :: char :: Nil => mkString(fpos, length, char)
        //        case l                     => PrimM[M].raiseError(PrimitiveArityError(name, 1, l.size))

        // case object `number->string` extends SchemePrim1("number->string"):
        //  def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
        //    for
        //      str <- unaryOp(SchemeOp.NumberToString)(x)
        //      adr <- PrimM[M].allocVal(fpos, str)
        //    yield lat.pointer(adr)

        // case object `string->number` extends SchemePrim1("string->number"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
        //         dereferencePointer(x) { (_, str) => lat.toNumber(str) }

        // case object `string->symbol` extends SchemePrim1("string->symbol"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
        //         dereferencePointer(x) { (_, str) => lat.toSymbol(str) }

        // case object `string-length` extends SchemePrim1("string-length"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
        //         dereferencePointer(x) { (_, str) => lat.length(x) }

        // case object `string-ref` extends SchemePrim2("string-ref"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V, idx: V): M[V] =
        //         dereferencePointer(x) { (_, str) => lat.ref(str, idx) }

        // case object `string-set!` extends SchemePrim3("string-set!"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V, idx: V, chr: V): M[V] =
        //         dereferencePointer(x) { (adr, str) =>
        //             for
        //                 updatedStr <- lat.set(str, idx, chr)
        //                 _ <- PrimM[M].updateSto(adr, updatedStr)
        //             yield unspecified
        //         }

        // case object `string<?` extends SchemePrim2("string<?"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V, y: V): M[V] =
        //         dereferencePointer(x) { (_, xstr) =>
        //             dereferencePointer(y) { (_, ystr) =>
        //                 lat.lt(xstr, ystr)
        //             }
        //         }

        // case object `string?` extends SchemePrimRefTypeCheck("string?", lat.isStr)

        // case object `substring` extends SchemePrim3("substring"):
        //     def call[M[_]: PrimM](fpos: SchemeExp, x: V, start: V, end: V): M[V] =
        //         for
        //             substr <- dereferencePointer(x) { (_, str) =>
        //                 lat.substring(str, start, end)
        //             }
        //             adr <- PrimM[M].allocVal(fpos, substr)
        //         yield lat.pointer(adr)

        // case object `symbol->string` extends SchemePrim1("symbol->string"):
        //   def call[M[_]: PrimM](fpos: SchemeExp, sym: V): M[V] =
        //     for
        //       str <- lat.toString[V](sym)
        //       adr <- PrimM[M].allocVal(fpos, str)
        //     yield lat.pointer(adr)

        // case object `char->string` extends SchemePrim1("char->string"):
        //   def call[M[_]: PrimM](fpos: SchemeExp, chr: V): M[V] =
        //     for
        //       str <- unaryOp(SchemeOp.CharacterToString)(chr)
        //       adr <- PrimM[M].allocVal(fpos, str)
        //     yield lat.pointer(adr)

        case object `cons` extends SchemePrim2("cons"):
            def call[M[_]: PrimM](fpos: SchemeExp, car: V, cdr: V): M[V] =
                for adr <- PrimM[M].storePair(fpos, dom.pairLattice.cons(car, cdr))
                yield lat.injectPairPtr(adr)

        case object `car` extends SchemePrim1("car"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
                dereferencePointer(x)(lat.pairAddress, "not a pair")((_, cons) => dom.pairLattice.car(cons))
        case object `cdr` extends SchemePrim1("cdr"):
            def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
                dereferencePointer(x)(lat.pairAddress, "not a pair")((_, cons) => dom.pairLattice.cdr(cons))

        case object `set-car!` extends SchemePrim2("set-car!"):
            def call[M[_]: PrimM](fpos: SchemeExp, cell: V, value: V): M[V] =
                dereferencePointer(cell)(lat.pairAddress, "not a pair") { (addr, cons) =>
                    for
                        cdr <- dom.pairLattice.cdr(cons)
                        _ <- PrimM[M].updateSto(addr, dom.pairLattice.cons(value, cdr))
                    yield unspecified
                }

        case object `set-cdr!` extends SchemePrim2("set-cdr!"):
            def call[M[_]: PrimM](fpos: SchemeExp, cell: V, value: V): M[V] =
                dereferencePointer(cell)(lat.pairAddress, "not a pair") { (addr, cons) =>
                    for
                        car <- dom.pairLattice.car(cons)
                        _ <- PrimM[M].updateSto(addr, dom.pairLattice.cons(car, value))
                    yield unspecified
                }

    case object `make-vector` extends SchemePrimVarArg("make-vector"):
        def createVec[M[_]: PrimM](fpos: SchemeExp, size: V, init: V): M[V] =
            for
                vec <- dom.vectorLattice.vector(size, init)
                adr <- PrimM[M].storeVec(fpos, vec)
            yield lat.injectVecPtr(adr)
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
            case size :: Nil         => createVec(fpos, size, unspecified)
            case size :: init :: Nil => createVec(fpos, size, init)
            case l =>
                PrimM[M].raiseError(PrimitiveVariadicArityError(name, 1, l.size))

    case object `vector` extends SchemePrimVarArg("vector"):
        def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] =
            for
                emptyVec <- dom.vectorLattice.vector(inject(BigInt(args.size))(using isogalois), bottom)
                filledVec <- args.zipWithIndex.foldLeftM[M, Vec](emptyVec) { case (acc, (arg, idx)) =>
                    dom.vectorLattice.vectorSet(acc, inject(BigInt(idx))(using isogalois), arg)
                }
                adr <- PrimM[M].storeVec(fpos, filledVec)
            yield lat.injectVecPtr(adr)

    case object `vector-length` extends SchemePrim1("vector-length"):
        def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
            dereferencePointer(x)(lat.vectorAddress, "not a vector") { (_, vec) => dom.vectorLattice.vectorLength(vec) }

    case object `vector-ref` extends SchemePrim2("vector-ref"):
        def call[M[_]: PrimM](fpos: SchemeExp, v: V, idx: V): M[V] =
            dereferencePointer(v)(lat.vectorAddress, "not a vector") { (_, vec) => dom.vectorLattice.vectorRef(vec, idx) }

    case object `vector-set!` extends SchemePrim3("vector-set!"):
        def call[M[_]: PrimM](fpos: SchemeExp, x: V, index: V, newval: V): M[V] =
            dereferencePointer(x)(lat.vectorAddress, "not a vector") { (adr, vec) =>
                for
                    newvec <- dom.vectorLattice.vectorSet(vec, index, newval)
                    _ <- PrimM[M].updateSto(adr, newvec)
                yield unspecified
            }

    // TODO: reintroduce structs in a proper way
    // case object StructRef extends SchemePrim2("__struct_ref"):
    //   def call[M[_]: PrimM](fpos: SchemeExp, s: V, field: V): M[V] =
    //     MonadJoin[M].mjoin(
    //       lat
    //         .getStructs(s)
    //         .flatMap(s =>
    //           // TODO: this is sound but very imprecise; use the value "field" and a corresponding operation in the lattice to make this more precise
    //           // and fetch the actual field whenever possible
    //           s.fields.contents.map(Monad[M].unit)
    //         ) ++ (if lat.isOpq(s) then
    //                 Set(Monad[M].unit(lat.opq(ContractValues.Opq())))
    //               else Set())
    //     )

// case object `input-port?` extends SchemePrim1("input-port?")
// case object `output-port?` extends SchemePrim1("output-port?")

// case object `open-input-file` extends SchemePrim1("open-input-file"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     dereferencePointer(x) { (_, str) =>
//       MonadJoin[M].condM(unaryOp(SchemeOp.IsString)(str)) {
//         for
//           // TODO: this could be cleaner by having a difference between a file input port and string input port, but this would be a bit overkill
//           portstring <- binaryOp(SchemeOp.StringAppend)(
//             string("__file__"),
//             str
//           )
//           inputPort <- unaryOp(SchemeOp.MakeInputPort)(portstring)
//         yield inputPort
//       } {
//         PrimM[M].raiseError(
//           PrimitiveNotApplicable("open-input-file", List(x))
//         )
//       }
//     }

// case object `open-input-string` extends SchemePrim1("open-input-string"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     dereferencePointer(x) { (_, str) =>
//       MonadJoin[M].condM(unaryOp(SchemeOp.IsString)(str)) {
//         unaryOp(SchemeOp.MakeInputPort)(str)
//       } {
//         PrimM[M].raiseError(
//           PrimitiveNotApplicable("open-input-string", List(x))
//         )
//       }
//     }

// case object `open-output-file` extends SchemePrim1("open-output-file"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     dereferencePointer(x) { (_, str) =>
//       MonadJoin[M].condM(unaryOp(SchemeOp.IsString)(str)) {
//         for
//           // TODO: this could be cleaner by having a difference between a file input port and string input port, but this would be a bit overkill
//           portstring <- binaryOp(SchemeOp.StringAppend)(
//             string("__file__"),
//             str
//           )
//           outputPort <- unaryOp(SchemeOp.MakeOutputPort)(portstring)
//         yield outputPort
//       } {
//         PrimM[M].raiseError(
//           PrimitiveNotApplicable("open-output-file", List(x))
//         )
//       }
//     }

// case object `close-input-port` extends SchemePrim1("close-input-port"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     MonadJoin[M].condM(unaryOp(SchemeOp.IsInputPort)(x)) {
//       unspecified.pure
//     } {
//       PrimM[M].raiseError(
//         PrimitiveNotApplicable("close-input-port", List(x))
//       )
//     }

// case object `close-output-port` extends SchemePrim1("close-output-port"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     MonadJoin[M].condM(unaryOp(SchemeOp.IsOutputPort)(x)) {
//       unspecified.pure
//     } {
//       PrimM[M].raiseError(
//         PrimitiveNotApplicable("close-output-port", List(x))
//       )
//     }

// case object `current-input-port` extends SchemePrim0("current-input-port"):
//   def call[M[_]: PrimM](fpos: SchemeExp): M[V] =
//     unaryOp(SchemeOp.MakeInputPort)(string("__console__"))

// case object `current-output-port`
//     extends SchemePrim0("current-output-port"):
//   def call[M[_]: PrimM](fpos: SchemeExp): M[V] =
//     unaryOp(SchemeOp.MakeOutputPort)(string("__console__"))

// class ReadOrPeekChar(name: String) extends SchemePrimVarArg(name):
//   def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = vs match
//     case Nil => charTop.pure
//     case inp :: Nil =>
//       MonadJoin[M].condM(unaryOp(SchemeOp.IsInputPort)(inp)) {
//         charTop.pure
//       } {
//         PrimM[M].raiseError(PrimitiveNotApplicable(name, vs))
//       }
//     case l => PrimM[M].raiseError(PrimitiveArityError(name, 1, l.size))

// case object `read-char` extends ReadOrPeekChar("read-char")
// case object `peek-char` extends ReadOrPeekChar("peek-char")

// case object `write-char` extends SchemePrimVarArg("write-char"):
//   def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] =
//     def check(res: M[V]): M[V] =
//       MonadJoin[M].condM(res) { unspecified.pure } {
//         PrimM[M].raiseError(PrimitiveNotApplicable(name, vs))
//       }
//     vs match
//       case chr :: Nil => check(unaryOp(SchemeOp.IsChar)(chr))
//       case chr :: out :: Nil =>
//         val res = for
//           isChar <- unaryOp(SchemeOp.IsChar)(chr)
//           isPort <- unaryOp(SchemeOp.IsOutputPort)(out)
//         yield and(isChar, isPort)
//         check(res)
//       case l => PrimM[M].raiseError(PrimitiveArityError(name, 2, l.size))

// class WriteOrDisplay(name: String) extends SchemePrimVarArg(name):
//   def call[M[_]: PrimM](fpos: SchemeExp, vs: List[V]): M[V] = vs match
//     case _ :: Nil => unspecified.pure
//     case _ :: outputPort :: Nil =>
//       MonadJoin[M].condM((unaryOp(SchemeOp.IsOutputPort)(outputPort))) {
//         unspecified.pure
//       } {
//         PrimM[M].raiseError(PrimitiveNotApplicable(name, vs))
//       }
//     case l => PrimM[M].raiseError(PrimitiveArityError(name, 1, l.size))

// case object `display` extends WriteOrDisplay("display")
// case object `write` extends WriteOrDisplay("write")

// case object `read` extends SchemePrimVarArg("read"):
//   def topValue[M[_]: PrimM](fpos: SchemeExp): M[V] =
//     for
//       adr <- PrimM[M].allocPtr(fpos)
//       ptr = lat.pointer(adr)
//       vlu = lat.join(
//         Seq(ptr, nil, numTop, realTop, charTop, symbolTop, boolTop)
//       )
//       cns = lat.cons(vlu, vlu)
//       // Note #1: creating a vector with these arguments is known to succeed
//       // Note #2: vector (and string) should use a different address than the cons-cell
//       // Note #3: need to ensure that abstract count == +inf for adr!
//       MayFailSuccess(vct) = lat.vector(numTop, vlu)
//       _ <- PrimM[M].extendSto(adr, cns)
//       _ <- PrimM[M].extendSto(adr, vct)
//       _ <- PrimM[M].extendSto(adr, stringTop)
//     yield vlu
//   def call[M[_]: PrimM](fpos: SchemeExp, args: List[V]): M[V] = args match
//     case Nil => topValue(fpos)
//     case inp :: Nil =>
//       MonadJoin[M].condM(unaryOp(SchemeOp.IsInputPort)(inp)) {
//         topValue(fpos)
//       } {
//         PrimM[M].raiseError(PrimitiveNotApplicable(name, args))
//       }
//     case oth => PrimM[M].raiseError(PrimitiveArityError(name, 1, oth.size))

// case object `eof-object?` extends SchemePrim1("eof-object?"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     /* TODO: there is no specific encoding for EOF objects, but they can only arise in scenarios where charTop is produced. So we can approximate them as follows */
//     if subsumes(x, charTop) then boolTop.pure
//     else inject(false).pure

// case object `new-lock` extends SchemePrim0("new-lock"):
//   def call[M[_]: PrimM](fpos: SchemeExp): M[V] =
//     for adr <- PrimM[M].allocVal(fpos, lat.lock(Set.empty))
//     yield lat.pointer(adr)

// case object `acquire` extends SchemePrim1("acquire"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     dereferencePointer(x) { (addr, lock) =>
//       for
//         thread <- PrimM[M].currentThread
//         locked <- lat.acquire(lock, thread)
//         _ <- PrimM[M].updateSto(addr, locked)
//       yield unspecified
//     }

// case object `release` extends SchemePrim1("release"):
//   def call[M[_]: PrimM](fpos: SchemeExp, x: V): M[V] =
//     dereferencePointer(x) { (addr, lock) =>
//       for
//         thread <- PrimM[M].currentThread
//         unlocked <- lat.release(lock, thread)
//         _ <- PrimM[M].updateSto(addr, unlocked)
//       yield unspecified
//     }

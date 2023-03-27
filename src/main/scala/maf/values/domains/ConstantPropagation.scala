package maf.values
package domains

import scala.annotation.tailrec
import cats.{MonadError => _, syntax => _, *}
import cats.data.*
import cats.extensions.*
import cats.syntax.all._
import typeclasses.*
import syntax.*
import maf.util.*

/** An implementation of a (rather standard) constant propagation domain. It
  * implements all the lattice typeclasses from `maf.values.typeclasses`.
  *
  * The domain consists of three values:
  *   - Bottom: the smallest element of the lattice, it is subsumed by any other
  *     value
  *   - Constant(x): denotes a constant value `x`
  *   - Top: denotes a non-constant value
  *
  * All operations are expressed using this lattice for simplicity. This means
  * that a `string-ref` operations is only supported for CP string values, and
  * CP int values, and no combination of other values.
  */
object ConstantPropagation:
  /** An alias to denote elements of the constant propagation domain */
  type CP[X] = L[X]

  /** All elements of the lattice are of type L
    *
    * @note
    *   The type parameter is covariant such that if Top <: L[Nothing] the
    *   following is true: L[Nothing] <: L[A] for any A
    */
  sealed trait L[+A]

  /** The non-constant value */
  case object Top extends L[Nothing]

  /** A constant value */
  case class Constant[A](x: A) extends L[A]

  /** The value is unknown */
  case object Bottom extends L[Nothing]

  /** Typeclass implementation of `Galois` */
  given [A]: Galois[A, L[A]] with
    def inject(a: A): L[A] = Constant(a)
    def extract(a: L[A]): Option[Set[A]] = a match
      case Top         => None
      case Bottom      => Some(Set())
      case Constant(v) => Some(Set(v))

  //
  // Extensions for convience
  //

  /** Converts a CP boolean to a boolean of the given abstract domain
    *
    * @tparam B
    *   resulting abstract value type
    */
  extension (b: L[Boolean])
    def toB[B: BoolLattice: GaloisFrom[Boolean]]: B = b match
      case Top         => BoolLattice[B].top
      case Bottom      => BoolLattice[B].bottom
      case Constant(b) => Galois.inject(b)

  /** Converts a CP integer to an integer of the given abstract domain
    *
    * @tparam I
    *   resulting abstract value type
    */
  extension (i: L[Int])
    def toI[I: IntLattice: GaloisFrom[BigInt]]: I = i match
      case Top         => IntLattice[I].top
      case Bottom      => IntLattice[I].bottom
      case Constant(n) => Galois.inject(BigInt(n))

  /** Map a function `f` over the value contained within `v`.
    *
    * @note
    *   If any of the elements is `Bottom` the result of the computation is also
    *   `Bottom`
    */
  extension [A](v: (L[A], L[A]))
    def mapN[B](f: (A, A) => B): L[B] = v match
      case (_, Bottom) | (Bottom, _)  => Bottom
      case (Top, _) | (_, Top)        => Top
      case (Constant(a), Constant(b)) => Constant(f(a, b))

  /** Map a function `f` over the value contained within `v` */
  extension [A](v: L[A])
    def map[B](f: A => B): L[B] = v match
      case Bottom      => Bottom
      case Top         => Top
      case Constant(a) => Constant(f(a))

  import Errors.*

  /** Provides base lattice operations for any constantly propagated value
    *
    * @param typeName
    *   the name of the type contained within the value, used for printing
    *   purposes.
    */
  abstract class BaseInstance[A: Show](typeName: String) extends Lattice[L[A]]:
    type Split = L[A]

    def show(x: L[A]): String = x match
      case Top         => typeName
      case Constant(x) => x.toString
      case Bottom      => s"$typeName.⊥"
    val bottom: L[A] = Bottom
    val top: L[A] = Top
    def join(x: L[A], y: => L[A]): L[A] = x match
      case Top => Top
      case Constant(_) =>
        y match
          case Top => Top
          case Constant(_) =>
            if x == y then x
            else Top
          case Bottom => x
      case Bottom => y
    def meet(x: L[A], y: => L[A]): L[A] = x match
      case Bottom => Bottom
      case Constant(_) =>
        y match
          case Top => x
          case Constant(_) =>
            if x == y then x
            else Bottom
          case Bottom => Bottom
      case Top => y
    def subsumes(x: L[A], y: => L[A]): Boolean = x match
      case Top => true
      case Constant(_) =>
        y match
          case Top         => false
          case Constant(_) => x == y
          case Bottom      => true
      case Bottom =>
        y match
          case Top         => false
          case Constant(_) => false
          case Bottom      => true
    def eql[B2: BoolLattice: GaloisFrom[Boolean]](n1: L[A], n2: L[A]): B2 =
      (n1, n2) match
        case (Top, Top)                 => BoolLattice[B2].top
        case (Top, Constant(_))         => BoolLattice[B2].top
        case (Constant(_), Top)         => BoolLattice[B2].top
        case (Constant(x), Constant(y)) => Galois.inject[Boolean, B2](x == y)
        case (Bottom, _)                => BoolLattice[B2].bottom
        case (_, Bottom)                => BoolLattice[B2].bottom

    def split(x: L[A]): Set[L[A]] = x match
      case Bottom            => Set()
      case Top | Constant(_) => Set(x)

  type B = L[Boolean]
  type S = L[String]
  type I = L[BigInt]
  type R = L[Double]
  type C = L[Char]
  type Sym = L[Symbol]

  /** A thin wrapper around strings and symbols in order to differentiate them
    * on a type level
    */
  case class Symbol(v: String) extends AnyVal

  object L:
    implicit val boolCP: BoolLattice[B] = new BaseInstance[Boolean]("Bool")
      with BoolLattice[B] {
      def inject(b: Boolean): B = Constant(b)
      def isTrue(b: B): Boolean = b match
        case Top         => true
        case Constant(x) => x
        case Bottom      => false
      def isFalse(b: B): Boolean = b match
        case Top         => true
        case Constant(x) => !x
        case Bottom      => false
      def not(b: B): B = b match
        case Top         => Top
        case Constant(x) => Constant(!x)
        case Bottom      => Bottom
    }

    implicit val stringCP: StringLattice[S, I, C, Sym] =
      new BaseInstance[String]("Str") with StringLattice[S, I, C, Sym] {
        def injectString(x: String): S = Constant(x)
        def length[M[_]: MonadError[Error]: MonadJoin](s: S): M[I] = (s match
          case Top         => IntLattice[I].top
          case Constant(s) => Galois.inject[BigInt, I](s.length)
          case Bottom      => IntLattice[I].bottom
        ).pure
        def append[M[_]: MonadError[Error]: MonadJoin](s1: S, s2: S): M[S] =
          ((s1, s2) match
            case (Bottom, _) | (_, Bottom)  => Bottom
            case (Top, _) | (_, Top)        => Top
            case (Constant(x), Constant(y)) => Constant(x ++ y)
          ).pure

        def makeString[M[_]: MonadError[Error]: MonadJoin](
            length: I,
            char: C
        ): M[S] = ((length, char) match
          case (Bottom, _) => stringCP.bottom.pure
          case (_, bot) if bot == charCP.bottom =>
            stringCP.bottom.pure
          case (Top, _) => stringCP.top.pure
          case (Constant(n), _) =>
            val c = charCP.toString(char)
            1.to(NumOps.bigIntToInt(n))
              .toList
              .foldM[M, S](Galois.inject(""))((s, _) => stringCP.append(s, c))
        )

        def substring[M[_]: MonadError[Error]: MonadJoin](
            s: S,
            from: I,
            to: I
        ): M[S] = ((s, from, to) match
          case (Bottom, _, _)                               => Bottom
          case (_, from, _) if IntLattice[I].isBottom(from) => Bottom
          case (_, _, to) if IntLattice[I].isBottom(to)     => Bottom
          case (Top, _, _)                                  => Top
          case (Constant(s), from, to)                      =>
            // This is duplicated code from ConcreteLattice, it should be refactored
            (0.to(s.size)
              .collect({
                case from2
                    if BoolLattice[B].isTrue(
                      IntLattice[I]
                        .eql[B](from, Galois.inject[BigInt, I](from2))
                    ) =>
                  (from2
                    .to(s.size)
                    .collect({
                      case to2
                          if BoolLattice[B].isTrue(
                            IntLattice[I]
                              .eql[B](to, Galois.inject[BigInt, I](to2))
                          ) =>
                        injectString(s.substring(from2, to2).nn)
                    }))
              })
              .flatten)
              .foldLeft(bottom)((s1, s2) => join(s1, s2))
        ).pure
        def ref[M[_]: MonadError[Error]: MonadJoin](s: S, i: I): M[C] = s match
          case Bottom => charCP.bottom.pure
          case Top    => charCP.top.pure
          case Constant(x) =>
            i match
              case Bottom => charCP.bottom.pure
              case Top =>
                charCP.top.pure || raiseError("string-ref: index out of bounds")
              case Constant(i) if i < x.length && i >= 0 =>
                Galois.inject(x(NumOps.bigIntToInt(i))).pure
              case _ => raiseError("string-ref: index ouf of bounds")

        def set[M[_]: MonadError[Error]: MonadJoin](
            s: S,
            i: I,
            c: C
        ): M[S] = s match
          case Bottom => Bottom.pure
          case _ if intCP.isBottom(i) || charCP.isBottom(c) =>
            Bottom.pure
          case Top => Top.pure
          case Constant(str) =>
            (i, c) match
              case (Constant(idx: BigInt), Constant(chr: Char)) =>
                Constant(str.updated(idx.toInt, chr)).pure
              // If neither the index or character are constant, don't even bother
              case _ => Top.pure
        def stringLt[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[
          Boolean
        ]](
            s1: S,
            s2: S
        ): M[B2] = ((s1, s2) match
          case (Bottom, _) | (_, Bottom)  => BoolLattice[B2].bottom
          case (Top, _) | (_, Top)        => BoolLattice[B2].top
          case (Constant(x), Constant(y)) => Galois.inject[Boolean, B2](x < y)
        ).pure
        def toSymbol[M[_]: MonadError[Error]: MonadJoin](s: S): M[Sym] =
          (s match
            case Bottom => SymbolLattice[Sym].bottom
            case Top    => SymbolLattice[Sym].top
          ).pure

        def toNumber[M[_]: MonadError[Error]: MonadJoin](s: S) =
          s match
            case Bottom => IntLattice[I].bottom.pure
            case Constant(str) =>
              Errors.liftFromOption[M](
                NumOps.bigIntFromString(str).map(Galois.inject[BigInt, I]),
                NotANumberString
              )
            case Top =>
              IntLattice[I].top.pure || ApplicativeError[M, Error].raiseError(
                NotANumberString
              )
      }

    implicit val intCP: IntLattice[I] = new BaseInstance[BigInt]("Int")
      with IntLattice[I] {
      def inject(x: BigInt): I = Constant(x)

      def toReal[M[_]: MonadError[
        Error
      ]: MonadJoin, R2: RealLattice: GaloisFrom[Double]](
          n: I
      ): M[R2] = (n match
        case Top         => RealLattice[R2].top
        case Constant(x) => Galois.inject[Double, R2](x.toDouble)
        case Bottom      => RealLattice[R2].bottom
      ).pure

      def random[M[_]: MonadError[Error]: MonadJoin](n: I): M[I] = (n match
        case Bottom => Bottom
        case _      => Top
      ).pure

      def plus[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I] =
        ((n1, n2) mapN { _ + _ }).pure
      def minus[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I] =
        (n1, n2).mapN { _ - _ }.pure
      def times[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I] =
        (n1, n2).mapN { _ * _ }.pure

      def div[M[_], R2: GaloisFrom[Double]](
          n1: I,
          n2: I
      )(using
          e1: cats.MonadError[M, Error],
          e2: maf.values.typeclasses.MonadJoin[M],
          e3: maf.values.typeclasses.RealLattice[R2]
      ): M[R2] = (n1, n2) match
        case (_, Top) =>
          RealLattice[R2].top.pure || raiseError("div: division by zero")
        case (Top, _) => RealLattice[R2].top.pure
        case (_, Constant(0)) =>
          raiseError("div: division by zero")
        case (Constant(x), Constant(y)) if y != 0 =>
          Galois
            .inject[Double, R2](
              NumOps.bigIntToDouble(x) / NumOps.bigIntToDouble(y)
            )
            .pure
        case _ => RealLattice[R2].bottom.pure

      def expt[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I] =
        ((n1, n2) mapN ((n1, n2) =>
          Math.pow(n1.toDouble, n2.toDouble).toInt
        ): I)
          .pure[M]

      override def quotient[M[_]: MonadError[Error]: MonadJoin](
          n1: I,
          n2: I
      ): M[I] =
        MonadJoin[M].cond(isZero(n2)) {
          raiseError("quotient: second argument is zero")
        } /* else */ {
          ((n1, n2) mapN { _ / _ }).pure
        }

      def modulo[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I] =
        MonadJoin[M].cond(isZero(n2)) {
          raiseError("modulo: second argument is 0")
        } /* else */ { ((n1, n2) mapN MathOps.modulo).pure }

      def remainder[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I] =
        MonadJoin[M].cond(isZero(n2)) {
          raiseError("remainder: second argument is 0")
        } /* else */ { ((n1, n2) mapN MathOps.remainder).pure }

      def lt[M[_]: MonadError[Error]: MonadJoin, B2: BoolLattice: GaloisFrom[
        Boolean
      ]](
          n1: I,
          n2: I
      ): M[B2] =
        ((n1, n2) mapN (_ < _)).toB[B2].pure

      def valuesBetween(n1: I, n2: I): Set[I] = (n1, n2) match
        case (Top, _)                   => Set(Top)
        case (_, Top)                   => Set(Top)
        case (Constant(x), Constant(y)) => x.to(y).map(i => Constant(i)).toSet
        case _                          => Set()

      def toString[Sym: SymbolLattice, I2: IntLattice, C: CharLattice_[
        I2,
        Sym,
        S
      ], S: StringLattice_[I2, C, Sym]: GaloisFrom[String]](n: I): S =
        n match
          case Top         => StringLattice[S, I2, C, Sym].top
          case Constant(x) => Galois.inject[String, S](x.toString)
          case Bottom      => StringLattice[S, I2, C, Sym].bottom
      def toChar[C: CharLattice_[I, Sym, S]: GaloisFrom[
        Char
      ], S: StringLattice_[
        I,
        C,
        Sym
      ], Sym: SymbolLattice](n: I): C = n match
        case Top => CharLattice[C, I, Sym, S].top
        case Constant(x) =>
          Galois.inject[Char, C](
            NumOps.bigIntToInt(x).asInstanceOf[Char]
          )
        case Bottom => CharLattice[C, I, Sym, S].bottom
    }

    implicit val realCP: RealLattice[R] = new BaseInstance[Double]("Real")
      with RealLattice[R] {
      def inject(x: Double) = Constant(x)
      def toInt[M[_]: MonadError[Error]: MonadJoin, I2: IntLattice: GaloisFrom[
        BigInt
      ]](
          n: R
      ): M[I2] =
        (n match
          case Top         => IntLattice[I2].top
          case Constant(x) => Galois.inject[BigInt, I2](x.toInt)
          case Bottom      => IntLattice[I2].bottom
        ).pure
      def ceiling[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n map (_.ceil)).pure
      def floor[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n map (_.floor)).pure
      def round[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n map (_.round): R).pure
      def random[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n match
          case Constant(_) => Top
          case _           => n
        ).pure

      def log[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond((n map (_ <= 0)).toB[B]) {
          raiseError("log of negative number")
        } /* else */ { (n map scala.math.log).pure }

      def sin[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n match
          case Constant(x) => Constant(scala.math.sin(x))
          case _           => n
        ).pure
      def asin[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond(n map (i => -1 <= i && i <= 1)) {
          (n map scala.math.asin).pure
        } /* else */ { raiseError("asin input must be in range of -1 and 1") }
      def cos[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n match
          case Constant(x) => Constant(scala.math.cos(x))
          case _           => n
        ).pure
      def acos[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond(n map (i => -1 <= i && i <= 1)) {
          (n map scala.math.acos).pure
        } /* else */ { raiseError("acos input must be in range of -1 and 1") }

      def tan[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        (n match
          case Constant(x) =>
            scala.math.tan(x) match
              case Double.NaN => raiseError("tan: out of bounds")
              case n          => Constant(n).pure
          case _ => n.pure
        )
      def atan[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] = (n match
        case Constant(x) => Constant(scala.math.atan(x))
        case _           => n
      ).pure
      def sqrt[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].condM(lt(n, inject(0))) {
          raiseError("sqrt input must be positive")
        } /* else */ { (n.map(Math.sqrt).pure) }
      def plus[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R] =
        (n1, n2).mapN { _ + _ }.pure
      def minus[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R] =
        (n1, n2).mapN { _ - _ }.pure
      def times[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R] =
        (n1, n2).mapN { _ * _ }.pure
      def div[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R] =
        MonadJoin[M].cond(isZero(n2)) {
          raiseError("division by zero")
        } /* else */ { ((n1, n2) mapN (_ / _)).pure }

      def expt[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R] =
        (n1, n2).mapN(Math.pow).pure
      def lt[M[_]: MonadError[Error]: MonadJoin, B2: BoolLattice: GaloisFrom[
        Boolean
      ]](
          n1: R,
          n2: R
      ): M[B2] =
        ((n1, n2) mapN (_ < _)).toB[B2].pure

      def toString[Sym2: SymbolLattice, I2: IntLattice, C2: CharLattice_[
        I2,
        Sym2,
        S
      ], S: StringLattice_[I2, C2, Sym2]: GaloisFrom[String]](c: R): S =
        c match
          case Top         => StringLattice[S, I2, C2, Sym2].top
          case Constant(x) => Galois.inject[String, S](x.toString)
          case Bottom      => StringLattice[S, I2, C2, Sym2].bottom
    }

    implicit val charCP: CharLattice[C, I, Sym, S] =
      new BaseInstance[Char]("Char") with CharLattice[C, I, Sym, S] {
        def inject(x: Char) = Constant(x)
        def downCase[M[_]: MonadError[Error]: MonadJoin](c: C): M[C] =
          c.map(_.toLower).pure
        def upCase[M[_]: MonadError[Error]: MonadJoin](c: C): M[C] =
          c.map(_.toUpper).pure

        def toString[Sym: SymbolLattice, I2: IntLattice, C2: CharLattice_[
          I2,
          Sym,
          S
        ], S: StringLattice_[I2, C2, Sym]: GaloisFrom[String]](c: C): S =
          c match
            case Top         => StringLattice[S, I2, C2, Sym].top
            case Constant(x) => Galois.inject[String, S](x.toString)
            case Bottom      => StringLattice[S, I2, C2, Sym].bottom

        def toInt[M[_]: MonadError[
          Error
        ]: MonadJoin, I2: IntLattice: GaloisFrom[BigInt]](
            c: C
        ): M[I2] = (c map (_.toInt)).toI[I2].pure
        def isLower[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[Boolean]](
            c: C
        ): M[B2] = (c map (_.isLower)).toB[B2].pure
        def isUpper[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[Boolean]](
            c: C
        ): M[B2] = (c map (_.isUpper)).toB[B2].pure
        override def charEq[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[Boolean]](
            c1: C,
            c2: C
        ): M[B2] =
          ((c1, c2) mapN (_ == _)).toB[B2].pure
        override def charLt[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[Boolean]](
            c1: C,
            c2: C
        ): M[B2] =
          ((c1, c2) mapN (_ < _)).toB[B2].pure
        override def charEqCI[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[Boolean]](
            c1: C,
            c2: C
        ): M[B2] =
          ((c1, c2) mapN (_.toUpper == _.toUpper)).toB[B2].pure
        override def charLtCI[M[_]: MonadError[
          Error
        ]: MonadJoin, B2: BoolLattice: GaloisFrom[Boolean]](
            c1: C,
            c2: C
        ): M[B2] =
          ((c1, c2) mapN { _.toUpper < _.toUpper }).toB[B2].pure
      }

    /** A symbol can be converted to a String implicitly */
    given Conversion[Symbol, String] = _.v

    given Show[Symbol] with
      def show(v: Symbol): String = v.v.toString()

    implicit val symCP: SymbolLattice[Sym] =
      new BaseInstance[Symbol]("Symbol") with SymbolLattice[Sym] {
        def injectSymbol(x: String) = Constant(x)

        def toString[Sym2: SymbolLattice, I2: IntLattice, C2: CharLattice_[
          I2,
          Sym2,
          S
        ], S: StringLattice_[I2, C2, Sym2]: GaloisFrom[String]](c: Sym): S =
          c match
            case Top                 => StringLattice[S, I2, C2, Sym2].top
            case Constant(Symbol(x)) => Galois.inject[String, S](x)
            case Bottom              => StringLattice[S, I2, C2, Sym2].bottom
      }

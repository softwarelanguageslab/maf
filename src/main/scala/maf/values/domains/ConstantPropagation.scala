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

/** Constant propagation lattice */
object ConstantPropagation:

  sealed trait L[+A]

  case object Top extends L[Nothing]

  case class Constant[A](x: A) extends L[A]

  case object Bottom extends L[Nothing]

  given [A]: CanEqual[L[A], L[A]] = CanEqual.derived

  extension (b: L[Boolean])
    def toB[B: BoolLattice]: B = b match
      case Top         => BoolLattice[B].top
      case Bottom      => BoolLattice[B].bottom
      case Constant(b) => BoolLattice[B].inject(b)

  extension (i: L[Int])
    def toI[I: IntLattice]: I = i match
      case Top         => IntLattice[I].top
      case Bottom      => IntLattice[I].bottom
      case Constant(n) => IntLattice[I].inject(n)

  extension [A](v: (L[A], L[A]))
    def mapN[B](f: (A, A) => B): L[B] = v match
      case (_, Bottom)                => Bottom
      case (Bottom, _)                => Bottom
      case (Top, _)                   => Top
      case (_, Top)                   => Top
      case (Constant(a), Constant(b)) => Constant(f(a, b))

  extension [A](v: L[A])
    def map[B](f: A => B): L[B] = v match
      case Bottom      => Bottom
      case Top         => Top
      case Constant(a) => Constant(f(a))

  given Monad[L] with
    def flatMap[A, B](fa: L[A])(f: (A) => L[B]): L[B] = fa match
      case Bottom      => Bottom
      case Top         => Top
      case Constant(a) => f(a)

    def pure[A](x: A): L[A] =
      Constant(x)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => L[Either[A, B]]): L[B] = f(
      a
    ) match
      case Bottom                => Bottom
      case Top                   => Top
      case Constant(Left(nextA)) => tailRecM(nextA)(f) // continue the recursion
      case Constant(Right(b))    => Constant(b) // recursion done

  abstract class BaseInstance[A: Show](typeName: String) extends Lattice[L[A]]:
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
    def eql[B2: BoolLattice](n1: L[A], n2: L[A]): B2 = (n1, n2) match
      case (Top, Top)                 => BoolLattice[B2].top
      case (Top, Constant(_))         => BoolLattice[B2].top
      case (Constant(_), Top)         => BoolLattice[B2].top
      case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x == y)
      case (Bottom, _)                => BoolLattice[B2].bottom
      case (_, Bottom)                => BoolLattice[B2].bottom

  type B = L[Boolean]
  type S = L[String]
  type I = L[BigInt]
  type R = L[Double]
  type C = L[Char]
  type Sym = L[String]

  private def raiseError[M[_]: MonadError[Error], A](msg: String): M[A] =
    ApplicativeError[M, Error].raiseError(StringError(msg))

  private def raiseError[M[_]: MonadError[Error], A](e: Error): M[A] =
    ApplicativeError[M, Error].raiseError(e)

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
        def inject(x: String): S = Constant(x)
        def length(s: S): I = s match
          case Top         => IntLattice[I].top
          case Constant(s) => IntLattice[I].inject(s.length)
          case Bottom      => IntLattice[I].bottom
        def append(s1: S, s2: S): S = (s1, s2) match
          case (Bottom, _) | (_, Bottom)  => Bottom
          case (Top, _) | (_, Top)        => Top
          case (Constant(x), Constant(y)) => Constant(x ++ y)

        def makeString(
            length: I,
            char: C
        ): S = (length, char) match
          case (Bottom, _) => stringCP.bottom
          case (_, bot) if bot == charCP.bottom =>
            stringCP.bottom
          case (Top, _) => stringCP.top
          case (Constant(n), _) =>
            val c = charCP.toString(char)
            1.to(NumOps.bigIntToInt(n))
              .foldLeft(stringCP.inject(""))((s, _) => stringCP.append(s, c))

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
                      IntLattice[I].eql[B](from, IntLattice[I].inject(from2))
                    ) =>
                  (from2
                    .to(s.size)
                    .collect({
                      case to2
                          if BoolLattice[B].isTrue(
                            IntLattice[I]
                              .eql[B](to, IntLattice[I].inject(to2))
                          ) =>
                        inject(s.substring(from2, to2).nn)
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
                charCP.inject(x(NumOps.bigIntToInt(i))).pure
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
        def lt[B2: BoolLattice](s1: S, s2: S): B2 = (s1, s2) match
          case (Bottom, _) | (_, Bottom)  => BoolLattice[B2].bottom
          case (Top, _) | (_, Top)        => BoolLattice[B2].top
          case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
        def toSymbol(s: S): Sym = s match
          case Bottom      => SymbolLattice[Sym].bottom
          case Top         => SymbolLattice[Sym].top
          case Constant(x) => SymbolLattice[Sym].inject(x)

        def toNumber[M[_]: MonadError[Error]: MonadJoin](s: S) =
          s match
            case Bottom => IntLattice[I].bottom.pure
            case Constant(str) =>
              Errors.liftFromOption[M](
                NumOps.bigIntFromString(str).map(IntLattice[I].inject),
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

      def toReal[R2: RealLattice](n: I): R2 = n match
        case Top         => RealLattice[R2].top
        case Constant(x) => RealLattice[R2].inject(x.toDouble)
        case Bottom      => RealLattice[R2].bottom

      def random(n: I): I = n match
        case Bottom => Bottom
        case _      => Top

      def plus(n1: I, n2: I): I = (n1, n2) mapN { _ + _ }
      def minus(n1: I, n2: I): I = (n1, n2) mapN { _ - _ }
      def times(n1: I, n2: I): I = (n1, n2) mapN { _ * _ }

      override def div[M[_]: MonadError[Error]: MonadJoin, R2: RealLattice](
          n1: I,
          n2: I
      ): M[R2] = (n1, n2) match
        case (_, Top) =>
          RealLattice[R2].top.pure || raiseError("div: division by zero")
        case (Top, _) => RealLattice[R2].top.pure
        case (_, Constant(0)) =>
          raiseError("div: division by zero")
        case (Constant(x), Constant(y)) if y != 0 =>
          RealLattice[R2]
            .inject(
              NumOps.bigIntToDouble(x) / NumOps.bigIntToDouble(y)
            )
            .pure
        case _ => RealLattice[R2].bottom.pure

      def expt(n1: I, n2: I): I =
        (n1, n2) mapN ((n1, n2) => Math.pow(n1.toDouble, n2.toDouble).toInt)

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

      def lt[B2: BoolLattice](n1: I, n2: I): B2 =
        ((n1, n2) mapN (_ < _)).toB[B2]

      def valuesBetween(n1: I, n2: I): Set[I] = (n1, n2) match
        case (Top, _)                   => Set(Top)
        case (_, Top)                   => Set(Top)
        case (Constant(x), Constant(y)) => x.to(y).map(i => Constant(i)).toSet
        case _                          => Set()

      def toString[C: CharLattice_[I, Sym, S], S: StringLattice_[
        I,
        C,
        Sym
      ], Sym: SymbolLattice](n: I): S = n match
        case Top         => StringLattice[S, I, C, Sym].top
        case Constant(x) => StringLattice[S, I, C, Sym].inject(x.toString)
        case Bottom      => StringLattice[S, I, C, Sym].bottom
      def toChar[C: CharLattice_[I, Sym, S], S: StringLattice_[
        I,
        C,
        Sym
      ], Sym: SymbolLattice](n: I): C = n match
        case Top => CharLattice[C, I, Sym, S].top
        case Constant(x) =>
          CharLattice[C, I, Sym, S].inject(
            NumOps.bigIntToInt(x).asInstanceOf[Char]
          )
        case Bottom => CharLattice[C, I, Sym, S].bottom
    }

    implicit val realCP: RealLattice[R] = new BaseInstance[Double]("Real")
      with RealLattice[R] {
      def inject(x: Double) = Constant(x)
      def toInt[I2: IntLattice](n: R): I2 = n match
        case Top         => IntLattice[I2].top
        case Constant(x) => IntLattice[I2].inject(x.toInt)
        case Bottom      => IntLattice[I2].bottom
      def ceiling(n: R): R = n map (_.ceil)
      def floor(n: R): R = n map (_.floor)
      def round(n: R): R = n map (_.round)
      def random(n: R): R = n match
        case Constant(_) => Top
        case _           => n
      def log[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond((n map (_ <= 0)).toB[B]) {
          raiseError("log of negative number")
        } /* else */ { (n map scala.math.log).pure }

      def sin(n: R): R = n match
        case Constant(x) => Constant(scala.math.sin(x))
        case _           => n
      def asin[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond(n map (i => -1 <= i && i <= 1)) {
          (n map scala.math.asin).pure
        } /* else */ { raiseError("asin input must be in range of -1 and 1") }
      def cos(n: R): R = n match
        case Constant(x) => Constant(scala.math.cos(x))
        case _           => n
      def acos[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond(n map (i => -1 <= i && i <= 1)) {
          (n map scala.math.acos).pure
        } /* else */ { raiseError("acos input must be in range of -1 and 1") }

      def tan(n: R): R =
        n match // TODO: use MayFail here for when x out of bounds
          case Constant(x) =>
            scala.math.tan(x) match
              case Double.NaN => Bottom
              case n          => Constant(n)
          case _ => n
      def atan(n: R): R = n match
        case Constant(x) => Constant(scala.math.atan(x))
        case _           => n
      def sqrt[M[_]: MonadError[Error]: MonadJoin](n: R): M[R] =
        MonadJoin[M].cond(lt(n, inject(0))) {
          raiseError("sqrt input must be positive")
        } /* else */ { (n map Math.sqrt).pure }
      def plus(n1: R, n2: R): R = (n1, n2) mapN { _ + _ }
      def minus(n1: R, n2: R): R = (n1, n2) mapN { _ - _ }
      def times(n1: R, n2: R): R = (n1, n2) mapN { _ * _ }
      def div[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R] =
        MonadJoin[M].cond(isZero(n2)) {
          raiseError("division by zero")
        } /* else */ { ((n1, n2) mapN (_ / _)).pure }

      def expt(n1: R, n2: R): R = (n1, n2) mapN Math.pow
      def lt[B2: BoolLattice](n1: R, n2: R): B2 =
        ((n1, n2) mapN (_ < _)).toB[B2]

      def toString[I: IntLattice, C: CharLattice_[I, Sym, S], S: StringLattice_[
        I,
        C,
        Sym
      ], Sym: SymbolLattice](n: R): S = n match
        case Top         => StringLattice[S, I, C, Sym].top
        case Constant(x) => StringLattice[S, I, C, Sym].inject(x.toString)
        case Bottom      => StringLattice[S, I, C, Sym].bottom
    }

    implicit val charCP: CharLattice[C, I, Sym, S] =
      new BaseInstance[Char]("Char") with CharLattice[C, I, Sym, S] {
        def inject(x: Char) = Constant(x)
        def downCase(c: C): C = c map (_.toLower)
        def upCase(c: C): C = c map (_.toUpper)
        def toString(c: C): S = c map (_.toString)
        def toInt[I2: IntLattice](c: C): I2 = (c map (_.toInt)).toI[I2]
        def isLower[B2: BoolLattice](c: C): B2 = (c map (_.isLower)).toB[B2]
        def isUpper[B2: BoolLattice](c: C): B2 = (c map (_.isUpper)).toB[B2]
        override def charEq[B2: BoolLattice](c1: C, c2: C): B2 =
          ((c1, c2) mapN (_ == _)).toB[B2]
        override def charLt[B2: BoolLattice](c1: C, c2: C): B2 =
          ((c1, c2) mapN (_ < _)).toB[B2]
        override def charEqCI[B2: BoolLattice](c1: C, c2: C): B2 =
          ((c1, c2) mapN (_.toUpper == _.toUpper)).toB[B2]
        override def charLtCI[B2: BoolLattice](c1: C, c2: C): B2 =
          ((c1, c2) mapN { _.toUpper < _.toUpper }).toB[B2]
      }

    implicit val symCP: SymbolLattice[Sym] =
      new BaseInstance[String]("Symbol")(LatticeShow.symShow)
        with SymbolLattice[Sym] {
        def inject(x: String) = Constant(x)
        def toString[I: IntLattice, C: CharLattice_[
          I,
          Sym,
          S
        ], S: StringLattice_[
          I,
          C,
          Sym
        ]](s: Sym): S = s match
          case Top         => StringLattice[S, I, C, Sym].top
          case Constant(x) => StringLattice[S, I, C, Sym].inject(x)
          case Bottom      => StringLattice[S, I, C, Sym].bottom
      }

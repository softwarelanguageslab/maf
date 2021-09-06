package maf.lattice

import maf.core._
import maf.lattice.interfaces._
import maf.util.Show
import maf.util.datastructures.SmartUnion._
import NumOps._

class ConcreteLattice:

    sealed trait L[+X]:

        def foldMap[Y: Lattice](f: X => Y): Y = this match
            case Top => Lattice[Y].top
            case Values(content) =>
              content.foldLeft(Lattice[Y].bottom)((acc, v) => Lattice[Y].join(acc, f(v)))
        def map[Y](f: X => Y): L[Y] = this match
            case Top             => Top
            case Values(content) => Values(content.map(f))
        def guardBot[Y: Lattice](body: => Y): Y = this match
            case Values(content) if content.isEmpty => Lattice[Y].bottom
            case _                                  => body

    case object Top extends L[Nothing]
    case class Values[X](values: Set[X]) extends L[X]

    def makeValues[X](contents: Set[X]): L[X] = Values(contents)

    abstract class BaseInstance[A: Show](typeName: String) extends Lattice[L[A]] with Serializable:
        def show(x: L[A]): String = x match
            case Top                                  => typeName
            case Values(content) if content.size == 1 => Show[A].show(content.head)
            case Values(content)                      => "{" + content.map(Show[A].show).mkString(",") + "}"
        val top: L[A] = Top
        val bottom: L[A] = Values[A](Set.empty)
        def join(x: L[A], y: => L[A]): L[A] = x match
            case Top => Top
            case Values(content1) =>
              y match
                  case Top              => Top
                  case Values(content2) => makeValues(sunion(content1.toSet[A], content2.toSet[A]))
        def subsumes(x: L[A], y: => L[A]): Boolean = x match
            case Top => true
            case Values(content1) =>
              y match
                  case Top              => false
                  case Values(content2) => content2.toSet[A].subsetOf(content1.toSet[A])
        def eql[B2: BoolLattice](x: L[A], y: L[A]): B2 = y.guardBot {
          x.foldMap(a => y.foldMap(b => BoolLattice[B2].inject(a == b)))
        }

    type S = L[String]
    type Sym = L[String]
    type B = L[Boolean]
    type I = L[BigInt]
    type R = L[Double]
    type C = L[Char]
    /* TODO[easy]: the bool lattice implementation could be specialized (see the old "ConcreteBoolEfficient" implementation). Whether this results in a speed improvement should be evaluated */

    object L:
        import maf.lattice._

        implicit class FoldMapOps[X](content: Set[X]):
            def foldMap[Y: Lattice](f: X => Y): Y =
              content.foldLeft(Lattice[Y].bottom)((acc, v) => Lattice[Y].join(acc, f(v)))

        implicit val stringConcrete: StringLattice[S] = new BaseInstance[String]("Str") with StringLattice[S] {
          def inject(x: String): S = makeValues(Set(x))
          def length[I2: IntLattice](s: S): I2 = s.foldMap(s => IntLattice[I2].inject(s.length))
          def append(s1: S, s2: S): S = (s1, s2) match
              case (Values(bot), _) if bot.isEmpty => makeValues(bot)
              case (_, Values(bot)) if bot.isEmpty => makeValues(bot)
              case (Top, _) | (_, Top)             => Top
              case (Values(content1), Values(content2)) =>
                makeValues(content1.foldMap(s1 => content2.map(s2 => s1 + s2)))
          def substring[I2: IntLattice](
              s: S,
              from: I2,
              to: I2
            ): S = (s, from, to) match
              case (Values(bot), _, _) if bot.isEmpty            => makeValues(bot)
              case (_, from, _) if from == IntLattice[I2].bottom => bottom
              case (_, _, to) if to == IntLattice[I2].bottom     => bottom
              case (Top, _, _) | (_, Top, _) | (_, _, Top)       => Top /* This could be further refined, but that wouldn't be too useful */
              case (Values(s), from, to)                         =>
                // Assumptions: from and to are in the string, we perform no bound check
                s.foldMap[S](s =>
                  (0.to(s.size)
                    .collect({
                      case from2 if BoolLattice[B].isTrue(IntLattice[I2].eql[B](from, IntLattice[I2].inject(from2))) =>
                        (from2
                          .to(s.size)
                          .collect({
                            case to2
                                if BoolLattice[B].isTrue(IntLattice[I2].eql[B](to, IntLattice[I2].inject(to2))) &&
                                  from2 <= to2 =>
                              inject(s.substring(from2, to2).nn)
                          }))
                    })
                    .flatten)
                    .foldLeft(bottom)((s1, s2) => join(s1, s2))
                )(stringConcrete)
          def ref[I2: IntLattice, C2: CharLattice](s: S, i: I2): C2 = s match
              case Values(bot) if bot.isEmpty => CharLattice[C2].bottom
              case Top                        => CharLattice[C2].top
              case Values(content)            =>
                /* Assumption: we don't perform any bound check. If i contains a value for which s has no character, it is silently ignored */
                content.foldMap(s =>
                  (0.to(s.size))
                    .collect({
                      case i2
                          if BoolLattice[B]
                            .isTrue(IntLattice[I2].eql[B](i, IntLattice[I2].inject(i2))) &&
                            i2 < s.size =>
                        CharLattice[C2].inject(s.charAt(i2))
                    })
                    .foldLeft(CharLattice[C2].bottom)((c1, c2) => CharLattice[C2].join(c1, c2))
                )
          def set[I2: IntLattice, C2: CharLattice](
              s: S,
              i: I2,
              c: C2
            ): S = s match
              case _ if s == bottom || i == IntLattice[I2].bottom || c == CharLattice[C2].bottom => bottom
              case _                                                                             => Top // TODO: more precise implementation
          def lt[B2: BoolLattice](s1: S, s2: S): B2 = (s1, s2) match
              case (Values(bot), _) if bot.isEmpty => BoolLattice[B2].bottom
              case (_, Values(bot)) if bot.isEmpty => BoolLattice[B2].bottom
              case (Top, _) | (_, Top)             => BoolLattice[B2].top
              case (Values(content1), Values(content2)) =>
                content1.foldMap(s1 => content2.foldMap(s2 => BoolLattice[B2].inject(s1 < s2)))
          def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = s.foldMap(s => SymbolLattice[Sym2].inject(s))
          def toNumber[I2: IntLattice](s: S): MayFail[I2, Error] = s match
              case Top => MayFail.success(IntLattice[I2].top).addError(NotANumberString)
              case Values(vs) =>
                vs.foldLeft(MayFail.success(IntLattice[I2].bottom): MayFail[I2, Error]) { (acc, str) =>
                  for
                      numv <- MayFail.fromOption[I2, Error](NumOps.bigIntFromString(str).map(IntLattice[I2].inject))(NotANumberString)
                      accv <- acc
                  yield IntLattice[I2].join(accv, numv)
                }
        }
        implicit val boolConcrete: BoolLattice[B] = new BaseInstance[Boolean]("Bool") with BoolLattice[B] {
          def inject(x: Boolean): B = makeValues(Set(x))
          def isTrue(b: B): Boolean = b match
              case Top             => true
              case Values(content) => content.contains(true)

          def isFalse(b: B): Boolean = b match
              case Top             => true
              case Values(content) => content.contains(false)

          def not(b: B): B = b.map(x => !x)
        }

        implicit val intConcrete: IntLattice[I] = new BaseInstance[BigInt]("Int") with IntLattice[I] {
          def inject(x: BigInt): I = makeValues(Set(x))

          def toReal[R2: RealLattice](n: I): R2 = n match
              case Top             => RealLattice[R2].top
              case Values(content) => content.foldMap((n: BigInt) => RealLattice[R2].inject(bigIntToDouble(n)))

          def random(n: I): I = if n == bottom then bottom else Top

          def plus(n1: I, n2: I): I = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 + n2)))

          def minus(n1: I, n2: I): I = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 - n2)))

          def times(n1: I, n2: I): I = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 * n2)))

          def quotient(n1: I, n2: I): I = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 / n2)))

          def div[R2: RealLattice](n1: I, n2: I): R2 = n2.guardBot {
            n1.foldMap(n1 => n2.foldMap(n2 => RealLattice[R2].inject(bigIntToDouble(n1) / bigIntToDouble(n2))))
          }
          def expt(n1: I, n2: I): I = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => Math.pow(n1.toDouble, n2.toDouble).toInt)))
          def modulo(n1: I, n2: I): I = n2.guardBot {
            n1.foldMap(n1 => n2.map(n2 => MathOps.modulo(n1, n2)))
          }
          def remainder(n1: I, n2: I): I = n2.guardBot {
            n1.foldMap(n1 => n2.map(n2 => MathOps.remainder(n1, n2)))
          }
          def lt[B2: BoolLattice](n1: I, n2: I): B2 = n2.guardBot {
            n1.foldMap(n1 => n2.foldMap(n2 => BoolLattice[B2].inject(n1 < n2)))
          }
          def valuesBetween(n1: I, n2: I): Set[I] = (n1, n2) match
              case (Top, _) => Set(Top)
              case (_, Top) => Set(Top)
              case (Values(vs1), Values(vs2)) =>
                if vs1.isEmpty || vs2.isEmpty then Set(makeValues(Set()))
                else vs1.min.to(vs2.max).map(i => makeValues(Set(i))).toSet
          def makeString[C2: CharLattice, S2: StringLattice](length: I, char: C2): S2 = (length, char) match
              case (bot, _) if bot == bottom                 => StringLattice[S2].bottom
              case (_, bot) if bot == CharLattice[C2].bottom => StringLattice[S2].bottom
              case (Top, _)                                  => StringLattice[S2].top
              case (Values(vs), _) =>
                val c = CharLattice[C2].toString[S2](char)
                vs.foldMap(n =>
                  /* Appends n times c to the empty string to construct the actual string we need */
                  1.to(NumOps.bigIntToInt(n)).foldLeft(StringLattice[S2].inject(""))((s, _) => StringLattice[S2].append(s, c))
                )

          def toString[S2: StringLattice](n: I): S2 =
            n.foldMap(n => StringLattice[S2].inject(n.toString))
          def toChar[C2: CharLattice](n: I): C2 =
            n.foldMap(n => CharLattice[C2].inject(NumOps.bigIntToInt(n).asInstanceOf[Char]))
        }

        implicit val realConcrete: RealLattice[R] = new BaseInstance[Double]("Real") with RealLattice[R] {
          def inject(x: Double): R = makeValues(Set(x))
          def toInt[I2: IntLattice](n: R): I2 = n.foldMap(n => IntLattice[I2].inject(n.toInt))
          def ceiling(n: R): R = n.map(_.ceil)
          def floor(n: R): R = n.map(_.floor)
          def round(n: R): R = n.map(n => MathOps.round(n))
          def log(n: R): R = n.map(n => scala.math.log(n))
          def random(n: R): R = if n == bottom then bottom else Top
          def sin(n: R): R = n.map(n => scala.math.sin(n))
          def asin(n: R): R = n.map(n => scala.math.asin(n))
          def cos(n: R): R = n.map(n => scala.math.cos(n))
          def acos(n: R): R = n.map(n => scala.math.acos(n))
          def tan(n: R): R =
            n.map(n => scala.math.sin(n) / scala.math.cos(n)) /* scala.math.tan isn't precise enough */
          def atan(n: R): R = n.map(n => scala.math.atan(n))
          def sqrt(n: R): R = n.map(n => scala.math.sqrt(n))
          def plus(n1: R, n2: R): R = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 + n2)))
          def minus(n1: R, n2: R): R = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 - n2)))
          def times(n1: R, n2: R): R = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 * n2)))
          def div(n1: R, n2: R): R = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => n1 / n2)))
          def expt(n1: R, n2: R): R = n2.guardBot(n1.foldMap(n1 => n2.map(n2 => Math.pow(n1, n2))))

          def lt[B2: BoolLattice](n1: R, n2: R): B2 = n2.guardBot {
            n1.foldMap(n1 => n2.foldMap(n2 => BoolLattice[B2].inject(n1 < n2)))
          }
          def toString[S2: StringLattice](n: R): S2 =
            n.foldMap(n => StringLattice[S2].inject(n.toString))
        }
        implicit val charConcrete: CharLattice[C] = new BaseInstance[Char]("Char") with CharLattice[C] {
          def inject(x: Char): C = makeValues(Set(x))
          def downCase(c: C): C = c.map(_.toLower)
          def upCase(c: C): C = c.map(_.toUpper)
          def toString[S2: StringLattice](c: C): S2 = c.foldMap(char => StringLattice[S2].inject(char.toString))
          def toInt[I2: IntLattice](c: C) = c.foldMap(c => IntLattice[I2].inject(c.toInt))

          def isLower[B2: BoolLattice](c: C): B2 = c.foldMap(char => BoolLattice[B2].inject(char.isLower))
          def isUpper[B2: BoolLattice](c: C): B2 = c.foldMap(char => BoolLattice[B2].inject(char.isUpper))

          def charEq[B2: BoolLattice](c1: C, c2: C): B2 = c2.guardBot(c1.foldMap(char1 => c2.foldMap(char2 => BoolLattice[B2].inject(char1 == char2))))
          def charLt[B2: BoolLattice](c1: C, c2: C): B2 = c2.guardBot(c1.foldMap(char1 => c2.foldMap(char2 => BoolLattice[B2].inject(char1 < char2))))

          // TODO: correct these definitions (see e.g. here: https://stackoverflow.com/questions/10223176/how-to-compare-character-ignoring-case-in-primitive-types)
          def charEqCI[B2: BoolLattice](c1: C, c2: C): B2 = c2.guardBot {
            c1.foldMap(char1 => c2.foldMap(char2 => BoolLattice[B2].inject(char1.toUpper == char2.toUpper)))
          }
          def charLtCI[B2: BoolLattice](c1: C, c2: C): B2 = c2.guardBot {
            c1.foldMap(char1 => c2.foldMap(char2 => BoolLattice[B2].inject(char1.toUpper < char2.toUpper)))
          }
        }
        implicit val symConcrete: SymbolLattice[Sym] = new BaseInstance[String]("Sym")(Show.symShow) with SymbolLattice[Sym] {
          def inject(x: String): Sym = makeValues(Set(x))
          def toString[S2: StringLattice](s: Sym): S2 = s.foldMap(s => StringLattice[S2].inject(s))
        }

object Concrete extends ConcreteLattice

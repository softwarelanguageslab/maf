package maf.lattice

import maf.lattice.interfaces.RealLattice
import maf.core.{Error, MayFail}
import maf.lattice.interfaces.IntLattice
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.StringLattice.apply
import maf.lattice.interfaces.StringLattice
import maf.lattice.interfaces.CharLattice

case class Nil()
case class Pai[H, T]()

object Definition:
    /** Computes the values of the given definitions, and joins them together */
    def joined(defs: Definition[_, _, _]*)(name: String)(args: List[HMap]): MayFail[HMap, Error] = ???
    // defs.foldMapL(_.compute(name)(args))

    extension (a: AbstractType)
        def ->(other: AbstractType): DefinitionArguments[other.AbstractValue, Pai[a.AbstractValue, Nil], Nothing] =
            DefinitionArguments(List(other, a))

        def -->(ret: AbstractType): DefinitionExpression[a.AbstractValue, Nil, ret.AbstractValue] =
            DefinitionExpression(List(a), ret)

sealed trait Fn[I, T, O]:
    def compute(name: String)(args: List[HMap], input: List[AbstractType]): MayFail[O, Error] =
        val allSet = args.zip(input).forall { case (arg, tpy) => arg.isSet(tpy) }
        if allSet then
            val allArgs = args.zip(input).map { case (arg, tpy) => arg.get(tpy) }
            MayFail.success(computeSpecialized(args))
        else MayFail.failure(maf.core.OperatorNotApplicable(name, args))

    protected def computeSpecialized(args: List[Any]): O

object Fn:
    implicit class Fn1[V, O](f: V => O) extends Fn[V, Nil, O]:
        export f.*
        def compute(name: String)(args: List[HMap], input: List[AbstractType]): MayFail[O, Error] =
            val allSet = args.zip(input).forall { case (arg, tpy) => arg.isSet(tpy) }
            if allSet then
                val allArgs = args.zip(input).map { case (arg, tpy) => arg.get(tpy) }
                MayFail.success(f(allArgs(0).asInstanceOf[V]))
            else MayFail.failure(maf.core.OperatorNotApplicable(name, args))

        protected def computeSpecialized(args: List[Any]): O =
            f(args(0).asInstanceOf[V])

    implicit class Fn2[V1, V2, O](f: (V2, V1) => O) extends Fn[V1, Pai[V2, Nil], O]:
        export f.*
        protected def computeSpecialized(args: List[Any]): O =
            f(args(0).asInstanceOf[V2], args(1).asInstanceOf[V1])

    implicit class Fn3[V1, V2, V3, O](f: (V3, V2, V1) => O) extends Fn[V1, Pai[V2, Pai[V3, Nil]], O]:
        export f.*
        protected def computeSpecialized(args: List[Any]): O =
            f(args(0).asInstanceOf[V3], args(1).asInstanceOf[V2], args(2).asInstanceOf[V1])

case class DefinitionArguments[I, T, O](input: List[AbstractType]):
    def ->(tpy: AbstractType): DefinitionArguments[tpy.AbstractValue, Pai[I, T], O] =
        this.copy(input = this.input :+ tpy)

    def -->(tpy: AbstractType): DefinitionExpression[I, T, tpy.AbstractValue] =
        DefinitionExpression(input = this.input, output = tpy)

case class DefinitionExpression[I, T, O](input: List[AbstractType], output: AbstractType):
    def |=|(expression: Fn[I, T, O]): Definition[I, T, O] = Definition(input, output, expression)

case class Definition[I, T, O](input: List[AbstractType], output: AbstractType, computation: Fn[I, T, O]):
    def apply(name: String)(args: List[HMap]): MayFail[HMap, Error] =
        computation.compute(name)(args, input).map(v => HMap.inserted(output, v.asInstanceOf[output.AbstractValue]))

class Tests[R: RealLattice, I: IntLattice, B: BoolLattice, S: StringLattice, C: CharLattice]:
    import Definition.*
    case object ReaT extends AType[R, Double]
    case object IntT extends AType[I, BigInt]
    case object BoolT extends AType[B, Boolean]
    case object StrT extends AType[S, String]
    case object CharT extends AType[C, Char]

    def coercebin(
        outputI: AbstractType,
        outputR: AbstractType
      )(
        intOp: (I, I) => outputI.AbstractValue,
        realOp: (R, R) => outputR.AbstractValue
      )(
        name: String
      )(
        args: List[HMap]
      ): MayFail[HMap, Error] =
        joined(
          (IntT -> IntT --> outputI |=| ((n1: I, n2: I) => intOp(n1, n2))),
          (ReaT -> ReaT --> outputR |=| ((r1: R, r2: R) => realOp(r1, r2))),
          (IntT -> ReaT --> outputR |=| ((n1: I, r2: R) => realOp(IntLattice[I].toReal[R](n1), r2))),
          (ReaT -> IntT --> outputR |=| ((r1: R, n2: I) => realOp(r1, IntLattice[I].toReal[R](n2))))
        )(name)(args)

    def minus = coercebin(IntT, ReaT)(
      IntLattice[I].minus(_, _),
      RealLattice[R].minus(_, _)
    )
    def times = coercebin(IntT, ReaT)(
      IntLattice[I].times(_, _),
      RealLattice[R].times(_, _)
    )
    def plus = coercebin(IntT, ReaT)(
      IntLattice[I].plus(_, _),
      RealLattice[R].plus(_, _)
    )
    def div = joined(
      (IntT -> IntT --> ReaT |=| ((r1: I, r2: I) => IntLattice[I].div(r1, r2))),
      (ReaT -> ReaT --> ReaT |=| ((r1: R, r2: R) => RealLattice[R].div(r1, r2))),
      (IntT -> ReaT --> ReaT |=| ((r1: I, r2: R) => RealLattice[R].div(IntLattice[I].toReal[R](r1), r2))),
      (ReaT -> IntT --> ReaT |=| ((r1: R, r2: I) => RealLattice[R].div(r1, IntLattice[I].toReal[R](r2))))
    )
    def quotient = IntT -> IntT --> IntT |=| ((r1: I, r2: I) => IntLattice[I].quotient(r1, r2))
    def expt = coercebin(IntT, ReaT)(
      IntLattice[I].expt(_, _),
      RealLattice[R].expt(_, _)
    )
    def modulo = (IntT -> IntT --> IntT |=| ((n1: I, n2: I) => IntLattice[I].modulo(n1, n2)))
    def remainder = (IntT -> IntT --> IntT |=| ((n1: I, n2: I) => IntLattice[I].remainder(n1, n2)))
    def lt = coercebin(BoolT, BoolT)(
      IntLattice[I].lt[B](_, _),
      RealLattice[R].lt[B](_, _)
    )
    def numEq = coercebin(BoolT, BoolT)(
      IntLattice[I].eql[B](_, _),
      RealLattice[R].eql[B](_, _)
    )
    def stringAppend = (StrT -> StrT --> StrT |=| ((s1: S, s2: S) => StringLattice[S].append(s1, s2)))
    def stringRef = (StrT -> IntT --> CharT |=| ((s1: S, i: I) => StringLattice[S].ref(s1, i)))
    def stringSet = (StrT -> IntT -> CharT --> StrT |=| ((s1: S, i: I, c: C) => StringLattice[S].set(s1, i, c)))
    def stringLt = (StrT -> StrT --> BoolT |=| ((s1: S, s2: S) => StringLattice[S].lt(s1, s2)))
    def characterEq = (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charEq(c1, c2)))
    def characterLt = (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charLt(c1, c2)))
    def characterEqCI = (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charEqCI(c1, c2)))
    def characterLtCI = (CharT -> CharT --> BoolT |=| ((c1: C, c2: C) => CharLattice[C].charLtCI(c1, c2)))
    def substring = (StrT -> IntT -> IntT --> StrT |=| ((s: S, from: I, to: I) => StringLattice[S].substring(s, from, to)))
    def makeString = (IntT -> CharT --> StrT |=| ((length: I, c: C) => IntLattice[I].makeString(length, c)))

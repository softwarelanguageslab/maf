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

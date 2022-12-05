package maf.test.language.scheme

import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalatest.propspec._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Shrink.*

import maf.core.Identifier
import maf.language.scheme.*
import maf.core.Identity


object ANFGenerator:

    type Id = String
    type AExp = SchemeLambda | SchemeVar 
    type CExp = SchemeLet | SchemeFuncall | AExp

    case class Context(scope: Set[Id]):
        def extendScope(nam: Id) = Context(scope + nam)
    lazy val emptyCtx: Context = Context(Set.empty)

    final private lazy val noId = Identity.none

    /** GENERATION **/ 

    private def fresh(ctx: Context): Gen[Id] = Gen.choose('a','z')
                                                  .map(_.toString)
                                                  .filterNot(ctx.scope.contains)

    def exp(ctx: Context = emptyCtx): Gen[CExp] = Gen.oneOf(appExp(ctx), letExp(ctx), aexp(ctx))

    def aexp(ctx: Context): Gen[AExp] = Gen.oneOf(varExp(ctx), lambdaExp(ctx))

    def varExp(ctx: Context): Gen[SchemeVar] = if ctx.scope.isEmpty
                                               then Gen.fail
                                               else Gen.oneOf(ctx.scope)
                                                       .map(nam => SchemeVar(Identifier(nam, noId)))

    def lambdaExp(ctx: Context): Gen[SchemeLambda] =
        for 
            par <- fresh(ctx)
            bdy <- exp(ctx.extendScope(par))
        yield SchemeLambda(None, List(Identifier(par, noId)), List(bdy), None, noId)
    
    def letExp(ctx: Context): Gen[SchemeLet] = 
        for 
            nam <- fresh(ctx)
            rhs <- exp(ctx)
            bdy <- exp(ctx.extendScope(nam))
        yield SchemeLet(List((Identifier(nam, noId), rhs)), List(bdy), noId)

    def appExp(ctx: Context): Gen[SchemeFuncall] =
        for 
            fun <- aexp(ctx)
            arg <- aexp(ctx)
        yield SchemeFuncall(fun, List(arg), noId)

    /** SHRINKING **/

    implicit def shrinkExp: Shrink[CExp] = Shrink[CExp] {
        case SchemeLet(List((nam, rhs: CExp @unchecked)), 
                       List(bdy: CExp @unchecked), 
                       idn) 
            => 
                // shrink the rhs ...
                (for rhsS <- shrink(rhs) yield SchemeLet(List((nam, rhsS)), List(bdy), idn))
                ++
                // ... or shrink the body ...
                (for bdyS <- shrink(bdy) yield SchemeLet(List((nam, rhs)), List(bdyS), idn))
                ++
                // ... or replace the let with either the rhs or the body
                Stream(rhs, bdy)
        case SchemeFuncall(fun: AExp @unchecked, 
                           List(arg: AExp @unchecked), 
                           idn) 
            => 
                // shrink the operator ...
                (for funS <- shrink(fun) yield SchemeFuncall(funS, List(arg), idn))
                ++
                // ... or shrink the operand ...
                (for argS <- shrink(arg) yield SchemeFuncall(fun, List(argS), idn))
                ++
                // ... or replace the call with either the operator or the operand
                Stream(fun, arg)
        case aexp: AExp => shrinkAExp.shrink(aexp)
    }

    implicit def shrinkAExp: Shrink[AExp] = Shrink[AExp] {
        case SchemeLambda(nam, prs, List(bdy: CExp @unchecked), ann, idn) => 
            for bdyS <- shrink(bdy) yield SchemeLambda(nam, prs, List(bdyS), ann, idn)
        case SchemeVar(_) => Stream.empty
    }

/** TODO[medium] tests for scheme lattice */

abstract class ANFGenSpecification extends AnyPropSpec with Checkers:
    // by default, check each property for at least 100 instances
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 10)
    def checkAll(props: Properties): Unit =
        for (name, prop) <- props.properties do property(name)(check(prop))
    def newProperties(name: String)(f: Properties => Properties): Properties =
        val p = new Properties(name)
        f(p)
    def conditional(p: Boolean, q: => Boolean): Boolean = !p || q

class Testje extends ANFGenSpecification:

    import ANFGenerator.*
    given Arbitrary[CExp] = Arbitrary(exp())

    val laws: Properties =
        newProperties("ANFLaws") { p =>
            p.property("foo") = forAll { (p: CExp) => false }
            p
        }
    checkAll(laws)
package maf.test.language.scheme

import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalatest.propspec._
import org.scalatestplus.scalacheck.Checkers

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

    implicit lazy val anyExp: Arbitrary[SchemeExp] = Arbitrary(exp())

    val laws: Properties =
        newProperties("ANFLaws") { p =>
            p.property("foo") = { println(exp().retryUntil(_ => true).sample) ; true }
            p
        }
    checkAll(laws)
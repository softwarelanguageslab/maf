package maf.test.language.scheme

import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalatest.propspec._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Shrink.*

import maf.core.Identifier
import maf.language.scheme.*
import maf.core.Identity
import maf.language.sexp


object SchemeExpGenerator:

    type Id = String
    type Exp = SchemeFuncall | SchemeLambda | SchemeVar | SchemeLet | SchemeLetrec | SchemeIf | SchemeValue 

    enum Typ:
        case Num
        case Bool
        case Fun(a: Typ, r: Typ)

    case class Context(scope: Map[Id, Typ]):
        def extendScope(nam: Id, typ: Typ) = Context(scope + (nam -> typ))
    lazy val emptyCtx: Context = Context(Map.empty)

    final private lazy val noId = Identity.none

    /** GENERATION **/ 

    private def fresh(ctx: Context): Gen[Id] = Gen.choose('a','z')
                                                  .map(_.toString)
                                                  .filterNot(ctx.scope.contains)

    def exp(typ: Typ = Typ.Num, ctx: Context = emptyCtx): Gen[Exp] = Gen.sized { depth =>
        def toggle(b: Boolean): Int = if b then 1 else 0
        val app = depth > 0
        val let = depth > 0
        val ltr = depth > 0
        val iff = depth > 0
        val lam = typ.isInstanceOf[Typ.Fun]
        val num = typ == Typ.Num 
        val bln = typ == Typ.Bool
        val vrb = ctx.scope.find((_, t) => t == typ).isDefined 
        Gen.frequency(
            toggle(app) -> appExp(typ, ctx), 
            toggle(let) -> letExp(typ, ctx), 
            toggle(ltr) -> letrecExp(typ, ctx),
            toggle(iff) -> iffExp(typ, ctx),
            toggle(lam) -> Gen.lzy(lambdaExp(typ.asInstanceOf[Typ.Fun], ctx)),
            toggle(num) -> numExp, 
            toggle(bln) -> blnExp,
            toggle(vrb) -> varExp(typ, ctx)
        )
    }

    private def sub[T](g: => Gen[T]): Gen[T] = 
        Gen.sized(depth => Gen.resize(depth - 1, g))

    private def subExp(typ: Typ, ctx: Context) = sub(exp(typ, ctx))

    def numExp: Gen[SchemeValue] = 
        Gen.chooseNum(0, 100).map(n => SchemeValue(sexp.Value.Integer(n), noId))
    
    def blnExp: Gen[SchemeValue] = 
        Gen.oneOf(true, false).map(b => SchemeValue(sexp.Value.Boolean(b), noId))

    def varExp(typ: Typ, ctx: Context): Gen[SchemeVar] = 
        val vrs = ctx.scope.filter((_, t) => t == typ)
        if vrs.isEmpty
        then Gen.fail
        else Gen.oneOf(ctx.scope).map((nam, _) => SchemeVar(Identifier(nam, noId)))

    def lambdaExp(funT: Typ.Fun, ctx: Context): Gen[SchemeLambda] =
        val Typ.Fun(argT, retT) = funT
        for 
            par <- fresh(ctx)
            bdy <- subExp(retT, ctx.extendScope(par, argT))
        yield SchemeLambda(None, List(Identifier(par, noId)), List(bdy), None, noId)

    def anyTyp: Gen[Typ] = Gen.oneOf(Gen.const(Typ.Num), funTyp)
    def funTyp: Gen[Typ.Fun] = 
        // for now, let's only allow num -> num function types
        Gen.const(Typ.Fun(Typ.Num, Typ.Num))
        // otherwise, this can be enabled, but the recursion will need to be controlled somehow ...
        //for 
        //    a <- anyTyp
        //    r <- anyTyp
        //yield Typ.Fun(a, r)
    
    def letExp(ret: Typ, ctx: Context): Gen[SchemeLet] = 
        for 
            nam <- fresh(ctx)
            typ <- anyTyp
            rhs <- subExp(typ, ctx)
            bdy <- subExp(ret, ctx.extendScope(nam, typ))
        yield SchemeLet(List((Identifier(nam, noId), rhs)), List(bdy), noId)

    def iffExp(ret: Typ, ctx: Context): Gen[SchemeIf] =
        for
            prd <- subExp(Typ.Bool, ctx)
            thn <- subExp(ret, ctx)
            els <- subExp(ret, ctx)
        yield SchemeIf(prd, thn, els, noId)

    def letrecExp(ret: Typ, ctx: Context): Gen[SchemeLetrec] = 
        for 
            nam <- fresh(ctx)
            // for now, we only generate lambdas as the rhs for a letrec
            atyp <- anyTyp
            rtyp <- anyTyp
            typ = Typ.Fun(atyp, rtyp)
            ext = ctx.extendScope(nam, typ)
            rhs <- sub(lambdaExp(typ.asInstanceOf[Typ.Fun], ext))
            bdy <- subExp(ret, ext)
        yield SchemeLetrec(List((Identifier(nam, noId), rhs)), List(bdy), noId)

    def appExp(ret: Typ, ctx: Context): Gen[SchemeFuncall] =
        for 
            atyp <- anyTyp
            fun <- subExp(Typ.Fun(atyp, ret), ctx)
            arg <- subExp(atyp, ctx)
        yield SchemeFuncall(fun, List(arg), noId)

    /** SHRINKING **/

    def shrinkExp: Shrink[Exp] = Shrink[Exp] {
        case SchemeLet(List((nam, rhs: Exp @unchecked)), List(bdy: Exp @unchecked), idn) => 
            // shrink the rhs ...
            (for rhsS <- shrink(rhs) yield SchemeLet(List((nam, rhsS)), List(bdy), idn))
            ++
            // ... or shrink the body ...
            (for bdyS <- shrink(bdy) yield SchemeLet(List((nam, rhs)), List(bdyS), idn))
            ++
            // ... or replace the let with either the rhs or the body
            Stream(rhs, bdy)
        case SchemeLetrec(List((nam, rhs: Exp @unchecked)), List(bdy: Exp @unchecked), idn) => 
            // shrink the rhs ...
            (for rhsS <- shrink(rhs) yield SchemeLet(List((nam, rhsS)), List(bdy), idn))
            ++
            // ... or shrink the body ...
            (for bdyS <- shrink(bdy) yield SchemeLet(List((nam, rhs)), List(bdyS), idn))
            ++
            // ... or replace the letrec with either the rhs or the body
            Stream(rhs, bdy)
        case SchemeFuncall(fun: Exp @unchecked, List(arg: Exp @unchecked), idn) => 
                // shrink the operator ...
                (for funS <- shrink(fun) yield SchemeFuncall(funS, List(arg), idn))
                ++
                // ... or shrink the operand ...
                (for argS <- shrink(arg) yield SchemeFuncall(fun, List(argS), idn))
                ++
                // ... or replace the call with either the operator or the operand
                Stream(fun, arg)
        case SchemeLambda(nam, prs, List(bdy: Exp @unchecked), ann, idn) => 
            // shrink the body of the lambda ...
            (for bdyS <- shrink(bdy) yield SchemeLambda(nam, prs, List(bdyS), ann, idn))
            ++
            // ... or replace the lambda with its body
            Stream(bdy)
        case SchemeVar(_) => 
            // try replacing the variable with a number
            Stream(SchemeValue(sexp.Value.Integer(42), noId))
        case SchemeValue(_, _) =>
            Stream.empty
        case SchemeIf(prd, thn: Exp, els: Exp, idn) =>
            (for prdS <- shrink(prd) yield SchemeIf(prdS, thn, els, idn))
            ++
            (for thnS <- shrink(thn) yield SchemeIf(prd, thnS, els, idn))
            ++
            (for elsS <- shrink(els) yield SchemeIf(prd, thn, elsS, idn))
            ++
            Stream(thn, els)
        case e => throw new Exception(s"unsupported expression: $e")
    }


abstract class SchemeExpGenSpecification extends AnyPropSpec with Checkers:
    // by default, check each property for at least 100 instances
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)
    def checkAll(props: Properties): Unit =
        for (name, prop) <- props.properties do property(name)(check(prop))
    def newProperties(name: String)(f: Properties => Properties): Properties =
        val p = new Properties(name)
        f(p)
    def conditional(p: Boolean, q: => Boolean): Boolean = !p || q

class Testje extends SchemeExpGenSpecification:

    import SchemeExpGenerator.*
    given Arbitrary[Exp] = Arbitrary(exp())

    val laws: Properties =
        newProperties("SchemeExp Laws") { p =>
            p.property("foo") = forAll { (p: Exp) =>
                println(p)
                p match 
                    case SchemeLetrec(List((Identifier(nam,_), rhs)), List(bdy), _) => 
                        bdy match
                            case SchemeFuncall(SchemeVar(Identifier(f, _)), List(SchemeValue(sexp.Value.Integer(_),_)), _) if f == nam => false
                            case _ => true
                    case _ => true     
            }
            p
        }
    checkAll(laws)
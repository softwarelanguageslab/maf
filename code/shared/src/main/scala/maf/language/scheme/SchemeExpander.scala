package maf.language.scheme

import maf.core.Identity
import maf.core.Position.apply
import maf.core.Position.SyntacticalPositionAst
import maf.core.PositionalIdentity
import maf.core.SimpleIdentity
import maf.language.sexp.SExp

object SchemeExpander:
    case class ExpansionContext(parent: Identity, compiler: (String) => SchemeExp)
    implicit class SchemeExpanderInterpolator(sc: StringContext)(using ctx: ExpansionContext):
        def expand(args: SExp*): SchemeExp =
            val prg = sc.s(args.map(arg => s"(syntax $arg ${arg.idn.pos.line} ${arg.idn.pos.col})"): _*)
            println(prg)
            retag(ctx.compiler(prg), ctx.parent)

    def retag(exp: SchemeExp, parent: Identity): SchemeExp =
        def astPosition(depth: Int, col: Int): Identity =
            SimpleIdentity(SyntacticalPositionAst(parent.pos, depth, col))

        def visit(exp: SchemeExp, depth: Int): SchemeExp = exp match
            case p: Product =>
                val arguments = p.productIterator.zip(p.productElementNames).zip(0 to p.productArity).map { case ((vlu, name), width) =>
                    if name == "idn" then astPosition(depth, width)
                    else
                        vlu match
                            case e: SchemeExp => visit(e, depth + 1)
                            case x            => x
                }

                p.getClass.getConstructors.nn
                    .filter(_.nn.getParameterCount == arguments.size)
                    .head
                    .nn
                    .newInstance(arguments.toSeq)
                    .asInstanceOf[SchemeExp]

        visit(exp, 0)

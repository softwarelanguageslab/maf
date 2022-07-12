package maf.language.scheme

import maf.core.{Identifier, Identity}
import maf.core.Position.apply
import maf.core.Position.SyntacticalPositionAst
import maf.core.PositionalIdentity
import maf.core.SimpleIdentity
import maf.language.sexp.*
import java.lang.reflect.Constructor

object SchemeExpander:
    case class ExpansionContext(parent: Identity)
    implicit class SchemeExpanderInterpolator(sc: StringContext)(using ctx: ExpansionContext):
        def expand(args: SExp*): SExp =
            val prg = sc.s(args.zip((0 to args.size)).map((arg, i) => s"$$_$i"): _*)
            val parsed = SExpParser.parse(prg)
            assert(parsed.size == 1)
            val replaced = replace(parsed.head, args.toList)
            retag(replaced, ctx.parent, args.toSet)

    extension (p: Product)
        private def arguments: Iterator[(String, Any)] =
            p.productElementNames.zip(p.productIterator)

        private def newInstance(arguments: List[Any]): p.type =
            val args = arguments.toList

            // look for a matching constructor, and select the first one
            p.getClass
                .getConstructors()
                .nn
                .map(_.nn)
                .filter(constructor =>
                    val tpys = constructor.getParameterTypes().nn.map(_.nn)
                    tpys.size == args.size && tpys.zip(args).forall { case (t: Class[_], a) => t.isAssignableFrom(a.getClass) }
                )
                .head
                .newInstance(args: _*)
                .asInstanceOf[p.type]

    private def replace(parsed: SExp, args: List[SExp]): SExp =
        def visit(sexp: SExp, args: Array[SExp]): SExp = sexp match
            case p: Product =>
                val arguments = p.arguments.map { case (name, vlu) =>
                    vlu match
                        case SExpId(Identifier(n, _)) if n.startsWith("$_") =>
                            val idx = n.split("\\$_").nn.apply(1).nn.toInt
                            args(idx)
                        case vlu: SExp => visit(vlu, args)
                        case v =>
                            v
                }
                p.newInstance(arguments.toList)

        visit(parsed, args.toArray)

    private def retag(exp: SExp, parent: Identity, args: Set[SExp]): SExp =
        def astPosition(depth: Int, col: Int): Identity =
            SimpleIdentity(SyntacticalPositionAst(parent.pos, depth, col))

        def visit(exp: SExp, depth: Int): SExp = exp match
            case p: SExp if args.contains(p) => p
            case p: Product =>
                val arguments = p.arguments.zip(0 to p.productArity).map { case ((name, vlu), width) =>
                    if name == "idn" then astPosition(depth, width)
                    else
                        vlu match
                            case e: SExp => visit(e, depth + 1)
                            case x       => x
                }

                p.newInstance(arguments.toList)

        visit(exp, 0)

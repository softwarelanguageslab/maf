package maf.semantics.scheme.interpreter

import maf.syntax.scheme.*
import maf.syntax.{Identifier, Position}

object ConcreteValues:

    trait Value

    sealed trait AddrInfo:
        def pos: Option[Position]

    trait Prim:
        val name: String

        def call(fexp: SchemeFuncall[Any], args: List[(SchemeExp[Any], Value)]): Value

    trait SimplePrim extends Prim:
        def call(args: List[Value], position: Option[Position]): Value

        def call(fexp: SchemeFuncall[Any], args: List[(SchemeExp[Any], Value)]): Value = call(args.map(_._2), fexp.pos)

    type Addr = (Int, AddrInfo)
    type Env = Map[String, Addr]
    type Store = Map[Addr, Value]

    object AddrInfo:
        case class VarAddr(vrb: Identifier) extends AddrInfo:
            def pos = vrb.pos
        case class PrmAddr(nam: String) extends AddrInfo:
            def pos = None
        case class PtrAddr(exp: SchemeExp[Any]) extends AddrInfo:
            def pos = exp.pos
        case class PtrIgnoreAddr(exp: SchemeExp[Any]) extends AddrInfo:
            def pos = exp.pos

    object Value:

        /* arises from undefined behavior */
        case class Undefined(pos: Option[Position]) extends Value:
            override def toString: String = "#<undef>"

        case class Clo(lambda: SchemeLambdaExp[Any], env: Env) extends Value:
            override def toString: String = s"#<procedure:${lambda.lambdaName}>"

        case class Primitive(p: String) extends Value:
            override def toString: String = s"#<primitive:$p>"

        case class Str(str: String) extends Value:
            override def toString: String = str

        case class Symbol(sym: String) extends Value:
            override def toString: String = s"'$sym"

        case class Integer(n: BigInt) extends Value:
            override def toString: String = n.toString

        case class Real(r: Double) extends Value:
            override def toString: String = r.toString

        case class Bool(b: Boolean) extends Value:
            override def toString: String = if b then "#t" else "#f"

        case class Pointer(v: Addr) extends Value:
            override def toString: String = s"#<ptr $v>"

        case class Character(c: Char) extends Value:
            override def toString: String = c match
                case ' '  => "#\\space"
                case '\n' => "#\\newline"
                case c    => s"#\\$c"

        case object Nil extends Value:
            override def toString: String = "'()"

        case class Cons(car: Value, cdr: Value) extends Value:
            override def toString: String = s"#<cons $car $cdr>"

        case class Vector(
            size: BigInt,
            elems: Map[BigInt, Value],
            init: Value)
            extends Value:
            override def toString: String = s"#<vector[size:$size]>"

        case class InputPort(port: Handle) extends Value:
            override def toString: String = s"#<input-port:$port>"

        case class OutputPort(port: Handle) extends Value:
            override def toString: String = s"#<output-port:$port>"

        case object EOF extends Value:
            override def toString: String = "#<eof>"

        case object Void extends Value:
            override def toString: String = "#<void>"

        /** An error as a value */
        case class Error(e: ProgramError) extends Value:
            override def toString: String = s"<#error: $e>"

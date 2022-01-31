package maf.language.scheme.interpreter

import maf.core.{Identifier, Identity}
import maf.core.Position.Position
import maf.language.scheme._

import scala.concurrent.Future

object ConcreteValues:

    trait Value

    sealed trait AddrInfo:
        def idn: Identity

    trait Prim:
        val name: String

        def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value

    trait SimplePrim extends Prim:
        def call(args: List[Value], position: Position): Value

        def call(fexp: SchemeFuncall, args: List[(SchemeExp, Value)]): Value = call(args.map(_._2), fexp.idn.pos)

    type Addr = (Int, AddrInfo)
    type Env = Map[String, Addr]
    type Store = Map[Addr, Value]

    object AddrInfo:
        case class VarAddr(vrb: Identifier) extends AddrInfo:
            def idn = vrb.idn
        case class PrmAddr(nam: String) extends AddrInfo:
            def idn = Identity.none
        case class PtrAddr(exp: SchemeExp) extends AddrInfo:
            def idn = exp.idn

    object Value:

        /* arises from undefined behavior */
        case class Undefined(idn: Identity) extends Value:
            override def toString: String = "#<undef>"

        case class Clo(lambda: SchemeLambdaExp, env: Env) extends Value:
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

        case class Thread(fut: Future[Value]) extends Value:
            override def toString: String = s"#<thread>"

        case class Lock(l: java.util.concurrent.locks.Lock) extends Value:
            override def toString: String = "#<lock>"

        case class CThread(tid: Int) extends Value:
            override def toString: String = s"#<thread:$tid>"

        case object EOF extends Value:
            override def toString: String = "#<eof>"

        case object Void extends Value:
            override def toString: String = "#<void>"

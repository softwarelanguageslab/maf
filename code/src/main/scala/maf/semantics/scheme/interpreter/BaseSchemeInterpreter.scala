package maf.semantics.scheme.interpreter

import maf.domain.Error
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.Preluded
import maf.syntax.sexp
import maf.syntax.sexp.{SExp, SExpId, SExpPair, SExpValue, SExpVector}
import maf.util.Timeout

case class ProgramError(msg: Error) extends Exception:
    override def toString: String = msg.toString

/** Common functionality for different Scheme interpreters, and interface methods needed for the primitives. */
trait BaseSchemeInterpreter[V]:
    this: ConcreteSchemePrimitives => // Needed for initialEnv and initialSto
    // TODO: Maybe not all these definitions need to be abstract as some can be shared with the CPS interpreter.

    import ConcreteValues._

    def run(
        program: SchemeExp[Mutation | Named] & Preluded,
        timeout: Timeout.T
      ): Value

    lazy val (initialEnv, initialSto) =
        val emptyEnv = Map.empty[String, Addr]
        val emptySto = Map.empty[Addr, Value]
        Primitives.allPrimitives.foldLeft((emptyEnv, emptySto)) { case ((env: Env, sto: Store), (name: String, _: Prim)) =>
            val addr = newAddr(AddrInfo.PrmAddr(name))
            (env + (name -> addr), sto + (addr -> Value.Primitive(name)))
        }

    // Both access to 'lastAddr' and 'store' should be synchronized on 'this'!
    var lastAddr = 0

    def newAddr(meta: AddrInfo): (Int, AddrInfo) =
        synchronized {
            lastAddr += 1
            (lastAddr, meta)
        }

    var store = Map[Addr, Value]()

    def extendStore(a: Addr, v: Value): Unit =
        synchronized {
            store = store + (a -> v)
        }

    def lookupStore(a: Addr): Value =
        synchronized {
            store(a)
        }

    def lookupStoreOption(a: Addr): Option[Value] =
        synchronized {
            store.get(a)
        }

    def setStore(s: Map[Addr, Value]): Unit =
        synchronized {
            store = s
        }

    /**
     * Allocate the given value on the address determined by the given Scheme expression.
     *
     * @param exp
     *   the scheme expression that determines the address of the given value
     * @param value
     *   the value to allocate in the store
     */
    def allocateVal(exp: SchemeExp[Mutation | Named], value: Value): Value.Pointer =
        val addr = newAddr(AddrInfo.PtrAddr(exp))
        extendStore(addr, value)
        Value.Pointer(addr)

    /** Allocate the given pair recursively in the store. */
    def allocateAll(exp: SchemeExp[Mutation | Named], value: Value): ConcreteValues.Value = value match
        case ConcreteValues.Value.Cons(car, cdr) =>
            val carAlloc = allocateAll(exp, car)
            val cdrAlloc = allocateAll(exp, cdr)
            allocateVal(exp, ConcreteValues.Value.Cons(carAlloc, cdrAlloc))
        case _ => value

    def allocateCons(
        exp: SchemeExp[Mutation | Named],
        car: Value,
        cdr: Value
      ): Value =
        allocateVal(exp, Value.Cons(car, cdr))

    def allocateStr(exp: SchemeExp[Mutation | Named], str: String): Value.Pointer =
        allocateVal(exp, Value.Str(str))

    def getString(addr: Addr): String = lookupStore(addr) match
        case Value.Str(str) => str
        case v              => throw new UnexpectedValueTypeException[Value](v)

    def makeList(values: List[(SchemeExp[Mutation | Named], Value)]): Value = values match
        case Nil                  => Value.Nil
        case (exp, value) :: rest => allocateCons(exp, value, makeList(rest))

    /** Signals an error in the program to the user. */
    def signalException[R](msg: Error): R = throw ProgramError(msg)

    val io: IO

    def evalSExp(sexp: SExp, exp: SchemeExp[Mutation | Named]): Value = sexp match
        case SExpId(id)          => Value.Symbol(id.name)
        case SExpValue(value, _) => evalLiteral(value, exp)
        case SExpPair(car, cdr, _) =>
            val carValue = evalSExp(car, exp)
            val cdrValue = evalSExp(cdr, exp)
            allocateCons(exp, carValue, cdrValue)
        case SExpVector(elements, _) =>
            val values = elements.map(evalSExp(_, exp))
            allocateVal(exp, Value.Vector(values.size, values.zipWithIndex.map((value, idx) => BigInt(idx) -> value).toMap, Value.Undefined(exp.pos)))

    def evalLiteral(lit: sexp.Value, exp: SchemeExp[Mutation | Named]): ConcreteValues.Value = lit match
        case maf.syntax.sexp.Value.String(s)    => allocateStr(exp, s)
        case maf.syntax.sexp.Value.Symbol(s)    => Value.Symbol(s)
        case maf.syntax.sexp.Value.Integer(n)   => Value.Integer(n)
        case maf.syntax.sexp.Value.Real(r)      => Value.Real(r)
        case maf.syntax.sexp.Value.Boolean(b)   => Value.Bool(b)
        case maf.syntax.sexp.Value.Character(c) => Value.Character(c)
        case maf.syntax.sexp.Value.Nil          => Value.Nil

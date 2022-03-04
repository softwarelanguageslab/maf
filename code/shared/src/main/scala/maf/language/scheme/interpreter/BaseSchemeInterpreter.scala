package maf.language.scheme.interpreter

import maf.language.change.CodeVersion.{New, Version}
import maf.language.scheme._
import maf.language.sexp
import maf.language.sexp.{SExp, SExpId, SExpPair, SExpValue}
import maf.util.benchmarks.Timeout
import maf.core.Error

case class ProgramError(msg: Error) extends Exception:
    override def toString: String = msg.toString

/** Common functionality for different Scheme interpreters, and interface methods needed for the primitives. */
trait BaseSchemeInterpreter[V]:
    this: ConcreteSchemePrimitives => // Needed for initialEnv and initialSto
    // TODO: Maybe not all these definitions need to be abstract as some can be shared with the CPS interpreter.

    import ConcreteValues._

    def run(
        program: SchemeExp,
        timeout: Timeout.T,
        version: Version = New
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
     * @param ignore
     *   whether to ignore the address for soundness tests
     */
    def allocateVal(exp: SchemeExp, value: Value, ignore: Boolean = false): Value.Pointer =
        val addr = newAddr(if ignore then AddrInfo.PtrIgnoreAddr(exp) else AddrInfo.PtrAddr(exp))
        extendStore(addr, value)
        Value.Pointer(addr)

    /** Allocate the given pair recursively in the store. */
    def allocateAll(exp: SchemeExp, value: Value): ConcreteValues.Value = value match
        case ConcreteValues.Value.Cons(car, cdr) =>
          val carAlloc = allocateAll(exp, car)
          val cdrAlloc = allocateAll(exp, cdr)
          allocateVal(exp, ConcreteValues.Value.Cons(carAlloc, cdrAlloc))
        case _ => value

    def allocateCons(
        exp: SchemeExp,
        car: Value,
        cdr: Value
      ): Value =
      allocateVal(exp, Value.Cons(car, cdr))

    def allocateStr(exp: SchemeExp, str: String): Value.Pointer =
      allocateVal(exp, Value.Str(str))

    def getString(addr: Addr): String = lookupStore(addr) match
        case Value.Str(str) => str
        case v              => throw new UnexpectedValueTypeException[Value](v)

    def makeList(values: List[(SchemeExp, Value)]): Value = values match
        case Nil                  => Value.Nil
        case (exp, value) :: rest => allocateCons(exp, value, makeList(rest))

    /** Signals an error in the program to the user. */
    def signalException[R](msg: Error): R = throw ProgramError(msg)

    val io: IO

    def evalSExp(sexp: SExp, exp: SchemeExp): Value = sexp match
        case SExpId(id)          => Value.Symbol(id.name)
        case SExpValue(value, _) => evalLiteral(value, exp)
        case SExpPair(car, cdr, _) =>
          val carValue = evalSExp(car, exp)
          val cdrValue = evalSExp(cdr, exp)
          allocateCons(exp, carValue, cdrValue)

    def evalLiteral(lit: sexp.Value, exp: SchemeExp): ConcreteValues.Value = lit match
        case maf.language.sexp.Value.String(s)    => allocateStr(exp, s)
        case maf.language.sexp.Value.Symbol(s)    => Value.Symbol(s)
        case maf.language.sexp.Value.Integer(n)   => Value.Integer(n)
        case maf.language.sexp.Value.Real(r)      => Value.Real(r)
        case maf.language.sexp.Value.Boolean(b)   => Value.Bool(b)
        case maf.language.sexp.Value.Character(c) => Value.Character(c)
        case maf.language.sexp.Value.Nil          => Value.Nil

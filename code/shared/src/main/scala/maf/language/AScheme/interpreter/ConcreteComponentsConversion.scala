package maf.language.AScheme.interpreter

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Address
import maf.core.Environment
import maf.core.Identifier
import maf.core.Identity
import maf.language.AScheme.ASchemeParser
import maf.language.AScheme.ASchemeValues.*
import maf.language.AScheme.interpreter.ASchemeInterpreterCallback
import maf.language.AScheme.interpreter.CPSASchemeInterpreter
import maf.language.AScheme.interpreter.ConcreteASchemeValues.*
import maf.language.ContractScheme.interpreter.ConcreteValues
import maf.language.ContractScheme.interpreter.ConcreteValues.Value
import maf.language.change.CodeVersion
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo
import maf.modular.scheme.modactor.SchemeModActorComponent
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.util.Logger
import maf.util.Monoid
import maf.util.MonoidImplicits.FoldMapExtension
import maf.util.Reader
import maf.util.benchmarks.Timeout
import maf.util.datastructures.MapOps.*

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import maf.modular.scheme.modactor.SchemeModActorSemantics

/**
 * This trait can be mixed in to provide support for keeping track of all the elements reported by the CPSAScheme callback. Additionally, it provides
 * support for converting a concrete actor to an abstract description in the state space of a standard ModActor component.
 */
trait ConcreteComponentsConversion:
    type Analysis <: SchemeModActorSemantics
    type M = Message[ConcreteValues.Value]

    class ConcreteState:
        var mailboxes: MapWithDefault[ConcreteActor, List[M]] = Map().useDefaultValue(List())
        var behs: MapWithDefault[ConcreteActor, List[Behavior]] = Map().useDefaultValue(List())
        var spawned: List[ConcreteActor] = List()

        def this(mailboxes: Map[ConcreteActor, List[M]], behs: Map[ConcreteActor, List[Behavior]], spawned: List[ConcreteActor]) =
            this()
            this.mailboxes = mailboxes.useDefaultValue(List())
            this.behs = behs.useDefaultValue(List())
            this.spawned = spawned

        def isEmpty: Boolean = mailboxes.isEmpty && behs.isEmpty && spawned.isEmpty

    object ConcreteState:
        given Monoid[ConcreteState] with
            def zero: ConcreteState = ConcreteState()
            def append(x: ConcreteState, y: => ConcreteState): ConcreteState =
                val mailboxes = List(x.mailboxes, y.mailboxes).toMapAppended
                val behs = List(x.behs, y.behs).toMapAppended
                val spawned = x.spawned ++ y.spawned
                ConcreteState(mailboxes, behs, spawned)

    class ConcreteStateCallback(state: ConcreteState) extends ASchemeInterpreterCallback:
        def sendMessage(receiver: ConcreteActor, msg: Message[Value], idn: Identity): Unit =
            state.mailboxes = state.mailboxes.update(receiver)(msg :: _)

        def become(self: ConcreteActor, beh: Behavior): Unit =
            state.behs = state.behs.update(self)(beh :: _)

        def spawn(newActor: ConcreteActor): Unit =
            state.spawned = newActor :: state.spawned

    protected def concreteToAbstractActor(concr: ConcreteActor): SchemeModActorComponent[Unit] =
        maf.modular.scheme.modactor.Actor(concr.initialBeh.removeEnv, concr.initialBeh.lexEnv, ())

    case class ComparisonVarAddr(ident: Identifier) extends Address:
        override def printable: Boolean = false
        override def idn: Identity = ident.idn

    case class ComparisonPtrAddr(exp: SchemeExp) extends Address:
        override def printable: Boolean = false
        override def idn: Identity = exp.idn

    // TODO: also convert addresses in the abstract domain in the same way
    protected def convertAddr(analysis: Analysis, adr: (Int, AddrInfo)): Address =
        adr._2 match
            case AddrInfo.VarAddr(vrb) => ComparisonVarAddr(vrb)
            case AddrInfo.PtrAddr(ptr) => ComparisonPtrAddr(ptr)

    protected def convertConcreteValue(analysis: Analysis, vlu: ConcreteValues.Value): analysis.Value = vlu match
        case Value.Undefined(_)  => analysis.lattice.nil
        case Value.Void          => analysis.lattice.void
        case Value.Clo(lam, env) => analysis.lattice.closure(lam, Environment.empty /* TODO */ )
        case Value.Primitive(p)  => analysis.lattice.primitive(p)
        case Value.Str(s)        => analysis.lattice.string(s)
        case Value.Symbol(s)     => analysis.lattice.symbol(s)
        case Value.Integer(i)    => analysis.lattice.number(i)
        case Value.Real(r)       => analysis.lattice.real(r)
        case Value.Bool(b)       => analysis.lattice.bool(b)
        case Value.Character(c)  => analysis.lattice.char(c)
        case Value.Nil           => analysis.lattice.nil
        case Value.Pointer(a)    => analysis.lattice.pointer(convertAddr(analysis, a))
        case Value.Cons(a, d)    => analysis.lattice.cons(convertConcreteValue(analysis, a), convertConcreteValue(analysis, d))
        case Value.Vector(siz, els, ini) =>
            analysis.lattice.vector(analysis.lattice.number(siz), convertConcreteValue(analysis, ini)).getOrElse(throw Exception("should not fail"))
        case c: ConcreteActor => analysis.lattice.actor(Actor(c.name, concreteToAbstractActor(c).removeEnv))
        case b: Behavior      => analysis.lattice.beh(b.removeEnv)
        case v                => throw new Exception(s"Unknown concrete value type: $v")

    protected def toMsg(analysis: Analysis)(m: analysis.Msg): Message[analysis.Value] =
        Message(analysis.getTag(m), analysis.getArgs(m))

    /**
     * Creates a concrete interpreter for the given program
     *
     * @param program
     *   the program to run concretely
     * @param cb
     *   a callback that is used to register the results of concrete interpreter
     */
    protected def createInterpreter(program: SchemeExp, cb: ASchemeInterpreterCallback): CPSASchemeInterpreter =
        given Logger.Logger = Logger.DisabledLog()
        CPSASchemeInterpreter(cbA = cb)

    protected def alertMsg(msg: String): Unit

    /**
     * Runs the program <code>concreteRuns</code> times and joins the results together.
     *
     * @param concreteRuns
     *   the number of concrete runs to execute
     * @param program
     *   the program to concretely execute
     * @param b
     *   the name of the benchmark program to run
     * @return
     *   a single concrete state that summarizes all concrete executions of the program
     */
    protected def runConcrete(concreteRuns: Int, program: SchemeExp, b: String): ConcreteState =
        (0 until concreteRuns).foldMap[ConcreteState] { _ =>
            // Run the conrete interpreter
            val concreteState = ConcreteState()
            try
                val concreteInterpreter = createInterpreter(program, ConcreteStateCallback(concreteState))
                concreteInterpreter.run(program, Timeout.start(Duration(10, SECONDS)), CodeVersion.New)
                concreteState
            catch
                case _: TimeoutException =>
                    alertMsg(s"Concrete evaluation of $b timed out")
                    concreteState
                case e =>
                    alertMsg(s"Concrete execution of $b encountered an error $e")
                    concreteState
        }

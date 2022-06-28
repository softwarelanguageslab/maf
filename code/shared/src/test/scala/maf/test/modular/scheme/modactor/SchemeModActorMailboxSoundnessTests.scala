package maf.test.modular.scheme.modactor

import maf.test.SchemeBenchmarkTests
import maf.language.scheme.*
import maf.core.Identity
import maf.language.change.CodeVersion
import scala.concurrent.duration._
import maf.language.AScheme.ASchemeValues.*
import maf.language.AScheme.interpreter.ConcreteASchemeValues.*
import maf.language.ContractScheme.interpreter.ConcreteValues
import maf.language.AScheme.interpreter.ASchemeInterpreterCallback
import maf.language.AScheme.interpreter.CPSASchemeInterpreter
import maf.language.ContractScheme.interpreter.ConcreteValues.Value
import maf.util.datastructures.MapOps.*
import maf.util.benchmarks.Timeout
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.AScheme.ASchemeParser
import maf.util.Reader
import maf.util.Logger
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeoutException
import maf.modular.scheme.modactor.SchemeModActorComponent
import maf.core.Environment
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo
import maf.core.{Address, Identifier}

/**
 * Test for checking the soundness of the mailbox abstractions.
 *
 * Soundness of the inner-turn sequential Scheme programs is checked by the sequential Scheme soundness tests.
 */
trait SchemeModActorMailboxSoundnessTests extends SchemeBenchmarkTests:
    type M = Message[ConcreteValues.Value]
    type Analysis = SimpleSchemeModActorAnalysis

    protected def parseProgram(filename: String): SchemeExp =
        SchemeBegin(ASchemeParser.parse(Reader.loadFile(filename)), Identity.none)

    protected def createInterpreter(program: SchemeExp, cb: ASchemeInterpreterCallback): CPSASchemeInterpreter =
        given Logger.Logger = Logger.DisabledLog()

        CPSASchemeInterpreter(cbA = cb)

    def concreteRuns: Int = 1

    class ConcreteState:
        var mailboxes: MapWithDefault[ConcreteActor, List[M]] = Map().useDefaultValue(List())
        var behs: MapWithDefault[ConcreteActor, List[Behavior]] = Map().useDefaultValue(List())
        var spawned: List[ConcreteActor] = List()

    class ConcreteStateCallback(state: ConcreteState) extends ASchemeInterpreterCallback:
        def sendMessage(receiver: ConcreteActor, msg: Message[Value], idn: Identity): Unit =
            state.mailboxes = state.mailboxes.update(receiver)(msg :: _)

        def become(self: ConcreteActor, beh: Behavior): Unit =
            state.behs = state.behs.update(self)(beh :: _)

        def spawn(newActor: ConcreteActor): Unit =
            state.spawned = newActor :: state.spawned

    protected def concreteToAbstractActor(concr: ConcreteActor): SchemeModActorComponent[Unit] =
        maf.modular.scheme.modactor.Actor(concr.initialBeh, concr.initialBeh.lexEnv, ())

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
        case c: ConcreteActor => analysis.lattice.actor(Actor(c.name, concreteToAbstractActor(c)))
        case b: Behavior      => analysis.lattice.beh(b)
        case v                => throw new Exception(s"Unknown concrete value type: $v")

    protected def toMsg(analysis: Analysis)(m: analysis.Msg): Message[analysis.Value] =
        Message(analysis.getTag(m), analysis.getArgs(m))

    protected def compareMailboxes(analysis: Analysis, concreteResults: ConcreteState): Int =
        val abstractMail: Map[SchemeModActorComponent[Unit], Set[Message[analysis.Value]]] =
            analysis.getMailboxes.keys
                .map(analysis.view)
                .map(_.removeContext)
                .map(_.removeEnv) // TODO: actually convert env properly
                .zip(analysis.getMailboxes.values.map(_.messages).map(_.map(toMsg(analysis))))
                .toMapJoined

        val concreteMail: Map[SchemeModActorComponent[Unit], Set[Message[analysis.Value]]] =
            concreteResults.mailboxes.keys
                .map(concreteToAbstractActor)
                .map(_.removeEnv) // TODO: actually convert env properly
                .zip(concreteResults.mailboxes.values.map(_.map(_.mapValues(convertConcreteValue(analysis, _))).toSet))
                .toMapJoined

        val matchedMessages = concreteMail.map { case (cmp, msgs) =>
            msgs.filter(cmsg =>
                println(s"getting $cmp in abstractmail: ${abstractMail.get(cmp)}")
                abstractMail
                    .get(cmp)
                    .exists(_.exists(msg => cmsg.vlus.zip(msg.vlus).forall { case (x, y) => analysis.lattice.subsumes(y, x) || x == y }))
            )
        }
        println("=======================")
        println(s"${abstractMail.keys}")
        println(s"${concreteMail.keys}")
        println("=======================")

        println("=======================")
        println(s"${abstractMail.values}")
        println(s"${concreteMail.values}")
        println("=======================")

        println(s"Size mismatched ${concreteMail.values.map(_.size).sum - matchedMessages.map(_.size).sum}, size matched ${matchedMessages
            .map(_.size)
            .sum}, size concrete ${concreteMail.values.map(_.size).sum}")
        matchedMessages.map(_.size).sum - concreteMail.values.map(_.size).sum

    protected def compareSpawnedActors(analysis: Analysis, concreteResults: ConcreteState): Int =
        val abstractActors: Set[SchemeModActorComponent[Unit]] = analysis.getMailboxes.keys.map(analysis.view).map(_.removeContext).toSet
        val concreteActors: Set[SchemeModActorComponent[Unit]] = concreteResults.spawned.map(concreteToAbstractActor).toSet
        concreteActors.size - abstractActors.size

    protected def compareBehaviors(analysis: Analysis, concreteResults: ConcreteState): Int =
        // TODO
        0

    protected def compareResults(analysis: Analysis, concreteResults: ConcreteState): Boolean =
        // Compare mailboxes against each other
        compareMailboxes(analysis, concreteResults) >= 0 &&
            /* Compare spawned actors */ compareSpawnedActors(analysis, concreteResults) >= 0 &&
            /* Compare changes in behavior */ compareBehaviors(analysis, concreteResults) >= 0

    override protected def onBenchmark(b: String): Unit =
        property(s"$b is sound in mailbox abstractions") {
            val program = parseProgram(b)
            val concreteState = ConcreteState()
            // Run the conrete interpreter
            try
                val concreteInterpreter = createInterpreter(program, ConcreteStateCallback(concreteState))
                concreteInterpreter.run(program, Timeout.start(Duration(10, SECONDS)), CodeVersion.New)
            catch
                case _: TimeoutException =>
                    alert(s"Concrete evaluation of $b timed out")
                case e => cancel(s"Analysis of $b encountered an error $e")

            // Run the abstract interpreter
            val analysis = new SimpleSchemeModActorAnalysis(program)
            analysis.analyze()

            // Compare the results
            assert(compareResults(analysis, concreteState), "the results of the analysis should subsume the results of the concrete interpreter")
        }

// class SchemeModActorMailboxSoundnessTestsAllBenchmarks extends SchemeModActorMailboxSoundnessTests:
//     override def benchmarks: Set[String] = SchemeBenchmarkPrograms.actors - "test/concurrentScheme/actors/soter/unsafe_send.scm"

package maf.test.modular.scheme.modactor

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
import maf.test.SchemeBenchmarkTests
import maf.util.Logger
import maf.util.Monoid
import maf.util.MonoidImplicits.FoldMapExtension
import maf.util.Reader
import maf.util.benchmarks.Timeout
import maf.util.datastructures.MapOps.*

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

/**
 * Test for checking the soundness of the mailbox abstractions.
 *
 * Soundness of the inner-turn sequential Scheme programs is checked by the sequential Scheme soundness tests.
 */
trait SchemeModActorMailboxSoundnessTests extends SchemeBenchmarkTests:
    type M = Message[ConcreteValues.Value]
    type Analysis = SimpleSchemeModActorAnalysis

    protected def parseProgram(filename: String): SchemeExp =
        ASchemeParser.parseProgram(Reader.loadFile(filename))

    protected def createInterpreter(program: SchemeExp, cb: ASchemeInterpreterCallback): CPSASchemeInterpreter =
        given Logger.Logger = Logger.DisabledLog()

        CPSASchemeInterpreter(cbA = cb)

    def concreteRuns: Int = 5

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

    protected def compareMailboxes(analysis: Analysis, concreteResults: ConcreteState): Int =
        val abstractMail: Map[SchemeModActorComponent[Unit], Set[Message[analysis.Value]]] = {
            val mailboxes = analysis.getMailboxes.toList
            val keys = mailboxes.map(_._1)
            val values = mailboxes.map(_._2)

            keys
                .map(analysis.view)
                .map(_.removeContext)
                .map(_.removeEnv) // TODO: actually convert env properly
                .zip(values.map(_.messages).map(_.map(toMsg(analysis))))
                .toMapJoined
        }

        val concreteMail: Map[SchemeModActorComponent[Unit], Set[Message[analysis.Value]]] = {
            val mailboxes = concreteResults.mailboxes.toList
            val keys = mailboxes.map(_._1)
            val values = mailboxes.map(_._2)

            keys
                .map(concreteToAbstractActor)
                .map(_.removeEnv) // TODO: actually convert env properly
                .zip(values.map(_.map(_.mapValues(convertConcreteValue(analysis, _))).toSet))
                .toMapJoined
        }

        val matchedMessages = concreteMail.map { case (cmp, msgs) =>
            msgs.filter(cmsg =>
                // println(s"getting $cmp with concrete mail $cmsg in abstractmail: ${abstractMail.get(cmp)}")
                val existsMatchingMessage = abstractMail
                    .get(cmp)
                    .exists(_.exists(msg => cmsg.vlus.zip(msg.vlus).forall { case (x, y) => analysis.lattice.subsumes(y, x) || x == y }))
                if !existsMatchingMessage then alert(s"no matching message for $cmsg")
                existsMatchingMessage
            )
        }
        //println("=======================")
        //println(s"${abstractMail.keys}")
        //println(s"${concreteMail.keys}")
        //println("=======================")

        //println("=======================")
        //println(s"${abstractMail.values}")
        //println(s"${concreteMail.values}")
        //println("=======================")

        //println(s"Size mismatched ${concreteMail.values.map(_.size).sum - matchedMessages.map(_.size).sum}, size matched ${matchedMessages
        //    .map(_.size)
        //    .sum}, size concrete ${concreteMail.values.map(_.size).sum}")
        matchedMessages.map(_.size).sum - concreteMail.values.map(_.size).sum

    protected def compareSpawnedActors(analysis: Analysis, concreteResults: ConcreteState): Int =
        val abstractActors: Set[SchemeModActorComponent[Unit]] =
            analysis.getMailboxes.keys.map(analysis.view).map(_.removeContext).map(_.removeEnv).toSet
        val concreteActors: Set[SchemeModActorComponent[Unit]] = concreteResults.spawned.map(concreteToAbstractActor).map(_.removeEnv).toSet
        abstractActors.size - concreteActors.size

    protected def compareBehaviors(analysis: Analysis, concreteResults: ConcreteState): Int =
        val _abstractBehaviors = analysis.getBehaviors.toList
        val abstractBehaviors: Map[SchemeModActorComponent[Unit], Set[Behavior]] =
            (_abstractBehaviors
                .map(_._1)
                .map(_.removeContext)
                .map(_.removeEnv))
                .zip(_abstractBehaviors.map(_._2))
                .toMap
                .withDefaultValue(Set())

        val _concreteBehaviors = concreteResults.behs.toList
        val concreteBehaviors: Map[SchemeModActorComponent[Unit], Set[Behavior]] =
            (_concreteBehaviors
                .map(_._1)
                .map(concreteToAbstractActor)
                .map(_.removeEnv))
                .zip(_concreteBehaviors.map(_._2.toSet))
                .toMap
                .withDefaultValue(Set())

        // we count how many concrete behaviors do not have any correspondig abstract behavior,
        // this number should equal zero for a sound analysis.
        concreteBehaviors.count { case (actor, behs) => (behs -- abstractBehaviors(actor)).size > 0 }

    protected def compareResults(analysis: Analysis, concreteResults: ConcreteState): Boolean =
        // Compare mailboxes against each other
        compareMailboxes(analysis, concreteResults) == 0 &&
            /* Compare spawned actors */ compareSpawnedActors(analysis, concreteResults) >= 0 &&
            /* Compare changes in behavior */ compareBehaviors(analysis, concreteResults) == 0

    override protected def onBenchmark(b: String): Unit =
        property(s"$b is sound in mailbox abstractions") {
            val program = parseProgram(b)

            val concreteState = (0 until concreteRuns).foldMap[ConcreteState] { _ =>
                // Run the conrete interpreter
                val concreteState = ConcreteState()
                try
                    val concreteInterpreter = createInterpreter(program, ConcreteStateCallback(concreteState))
                    concreteInterpreter.run(program, Timeout.start(Duration(10, SECONDS)), CodeVersion.New)
                    concreteState
                catch
                    case _: TimeoutException =>
                        alert(s"Concrete evaluation of $b timed out")
                        concreteState
                    case e =>
                        alert(s"Concrete execution of $b encountered an error $e")
                        ConcreteState() // cancel(s"Analysis of $b encountered an error $e")
            }

            assert(!concreteState.isEmpty, "Concrete execution should have message sends")
            // Run the abstract interpreter
            val analysis = new SimpleSchemeModActorAnalysis(program)
            analysis.analyze()

            // Compare the results
            assert(compareResults(analysis, concreteState), "the results of the analysis should subsume the results of the concrete interpreter")
        }

class SchemeModActorMailboxSoundnessTestsAllBenchmarks extends SchemeModActorMailboxSoundnessTests:
    override def benchmarks: Set[String] = // Set("test/concurrentScheme/actors/soter/concdb.scm")
        // Set("test/concurrentScheme/actors/savina/trapr.scm")
        SchemeBenchmarkPrograms.actors - "test/concurrentScheme/actors/soter/unsafe_send.scm"

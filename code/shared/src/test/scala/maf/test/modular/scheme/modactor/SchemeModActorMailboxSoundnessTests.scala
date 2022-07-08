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
import maf.language.AScheme.interpreter.ConcreteComponentsConversion

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import maf.core.Lattice

/**
 * Test for checking the soundness of the mailbox abstractions.
 *
 * Soundness of the inner-turn sequential Scheme programs is checked by the sequential Scheme soundness tests.
 */
trait SchemeModActorMailboxSoundnessTests extends SchemeBenchmarkTests, ConcreteComponentsConversion:
    type Analysis = SimpleSchemeModActorAnalysis

    protected def parseProgram(filename: String): SchemeExp =
        ASchemeParser.parseProgram(Reader.loadFile(filename))

    def concreteRuns: Int = 5

    protected def compareMailboxes(analysis: Analysis, concreteResults: ConcreteState[analysis.Value]): Int =
        type AM = Set[Message[analysis.Value]]
        val abstractMail: Map[SchemeModActorComponent[Unit], Set[Message[analysis.Value]]] = {
            val mailboxes = analysis.getMailboxes.toList
            val keys = mailboxes.map(_._1)
            val values = mailboxes.map(_._2)

            keys
                .map(analysis.view)
                .map(_.removeContext)
                .map(_.removeEnv) // TODO: actually convert env properly
                .zip(
                  values
                      .map(_.messages)
                      .map(msgs =>
                          given Lattice[AM] = Message.messageSetLattice(analysis.lattice)
                          Lattice.foldMapL[analysis.Msg, AM](msgs, toMsg(analysis) andThen (Set() + _))
                      )
                )
                .toMapJoined
        }

        val concreteMail: Map[SchemeModActorComponent[Unit], Set[Message[analysis.Value]]] = {
            val mailboxes = concreteResults.mailboxes.toList
            val keys = mailboxes.map(_._1)
            val values = mailboxes.map(_._2)

            keys
                .zip(values)
                .toMapJoined
        }

        val matchedMessages = concreteMail.map { case (cmp, msgs) =>
            msgs.filter(cmsg =>
                //println(s"getting $cmp with concrete mail $cmsg in abstractmail: ${abstractMail.get(cmp)}")
                val existsMatchingMessage = abstractMail
                    .get(cmp)
                    .exists(_.exists(msg => cmsg.vlus.zip(msg.vlus).forall { case (x, y) => analysis.lattice.subsumes(y, x) || x == y }))
                if !existsMatchingMessage then alert(s"no matching message for $cmsg")
                existsMatchingMessage
            )
        }

        matchedMessages.map(_.size).sum - concreteMail.values.map(_.size).sum

    protected def compareSpawnedActors(analysis: Analysis, concreteResults: ConcreteState[analysis.Value]): Int =
        val abstractActors: Set[SchemeModActorComponent[Unit]] =
            analysis.getMailboxes.keys.map(analysis.view).map(_.removeContext).map(_.removeEnv).toSet
        val concreteActors: Set[SchemeModActorComponent[Unit]] = concreteResults.spawned.map(concreteToAbstractActor).map(_.removeEnv).toSet
        abstractActors.size - concreteActors.size

    protected def compareBehaviors(analysis: Analysis, concreteResults: ConcreteState[analysis.Value]): Int =
        val _abstractBehaviors = analysis.getBehaviors.toList
        val abstractBehaviors: Map[SchemeModActorComponent[Unit], Set[Behavior]] =
            (_abstractBehaviors
                .map(_._1)
                .map(_.removeContext)
                .map(_.removeEnv))
                .zip(_abstractBehaviors.map(_._2.map(_.removeEnv)))
                .toMap
                .withDefaultValue(Set())

        val _concreteBehaviors = concreteResults.behs.toList
        val concreteBehaviors: Map[SchemeModActorComponent[Unit], Set[Behavior]] =
            (_concreteBehaviors
                .map(_._1)
                .map(concreteToAbstractActor)
                .map(_.removeEnv))
                .zip(_concreteBehaviors.map(_._2.map(_.removeEnv).toSet))
                .toMap
                .withDefaultValue(Set())

        //println(concreteBehaviors.values)
        //println("================")
        //print("conc ")
        //println(concreteBehaviors.toList.map { case (actor, behs) => (behs, abstractBehaviors(actor)) })

        // we count how many concrete behaviors do not have any correspondig abstract behavior,
        // this number should equal zero for a sound analysis.
        concreteBehaviors.count { case (actor, behs) => (behs -- abstractBehaviors(actor)).size > 0 }

    protected def compareResults(analysis: Analysis, concreteResults: ConcreteState[analysis.Value]): Boolean =
        // Compare mailboxes against each other
        compareMailboxes(analysis, concreteResults) == 0 &&
            /* Compare spawned actors */ compareSpawnedActors(analysis, concreteResults) >= 0 &&
            /* Compare changes in behavior */ compareBehaviors(analysis, concreteResults) == 0

    protected def alertMsg(msg: String): Unit = alert(msg)

    override protected def onBenchmark(b: String): Unit =
        property(s"$b is sound in mailbox abstractions") {
            val program = parseProgram(b)

            val analysis = new SimpleSchemeModActorAnalysis(program)
            val concreteState = runConcrete(analysis, concreteRuns, program, b)

            //assert(!concreteState.isEmpty, "Concrete execution should have message sends")
            // Run the abstract interpreter
            analysis.analyze()

            // Compare the results
            assert(compareResults(analysis, concreteState), "the results of the analysis should subsume the results of the concrete interpreter")
        }

class SchemeModActorMailboxSoundnessTestsAllBenchmarks extends SchemeModActorMailboxSoundnessTests:
    override def benchmarks: Set[String] = // Set("test/concurrentScheme/actors/soter/concdb.scm")
        SchemeBenchmarkPrograms.actors --
            Set("test/concurrentScheme/actors/soter/unsafe_send.scm", "test/concurrentScheme/actors/savina/qsort.scm")

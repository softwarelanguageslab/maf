package maf.aam

import maf.util.graph.*
import scala.collection.immutable.HashSet
import maf.util.Trampoline.run
import maf.aam.scheme.AAMPeformanceMetrics
import maf.core.Expression

trait BaseSimpleWorklistSystem[E <: Expression] extends AAMAnalysis[E], AAMPeformanceMetrics[E]:
    val enableGraph: Boolean = false

    trait SeenStateSystem extends BaseSystem:
        this: System =>
        var seen: HashSet[Conf] = HashSet()
        var work: List[(Option[Conf], Conf)] = List()
        var bumps: Set[(Conf, Conf)] = Set()
        var newWork: List[(Option[Conf], Conf)] = List()

        def popWork(): Option[(Option[Conf], Conf)] =
          if work.nonEmpty then
              change {
                val conf = work.head
                work = work.tail
                Some(conf)
              }
          else if newWork.nonEmpty then
              change {
                val conf = newWork.head
                // return work from new work and swap work and new work
                val keep = work
                work = newWork.tail
                newWork = keep
                Some(conf)
              }
          else None

        def pushWork(prev: Option[Conf], work: Conf): this.type = change {
          newWork = (prev, work) :: newWork
          this
        }

        def addSeen(work: Conf): Unit = change {
          report(Seen, seen.size) // logging
          seen = seen + work
        }

        def addBump(from: Conf, to: Conf): Unit = change {
          bumps = bumps + (from -> to)
        }

        def allConfs: Set[Conf] = seen

        def finalStates: Set[State] =
          seen.map(asState(_, this)).filter(isFinal)

    type System <: SeenStateSystem

    protected def integrate(st: State, sys: System): (Conf, System) =
      (asConf(st, sys), sys)

    protected def decideSuccessors[G](depGraph: G, prev: Conf, successors: Set[State], sys: System)(using g: AAMGraph[G]): (System, G) =
        var depGraph2 = depGraph
        var sys2 = sys
        successors.foreach { successor =>
            val (conf, sys1) = integrate(successor, sys)
            if !sys1.seen.contains(conf) then
                sys1.pushWork(Some(prev), conf)
                sys1.addSeen(conf)
            else
                increment(Bump) // logging
                val n1 = asGraphElement(prev, sys1)
                val n2 = asGraphElement(conf, sys1)
                depGraph2 = g.addEdge(depGraph2, n1, NoTransition(), n2)

            sys2 = sys1
        }

        (sys2, depGraph2)

    override protected def transition[G](system: System, dependencyGraph: G)(using g: AAMGraph[G]): (System, G) =
        //println(s"seen ${system.seen.size}, work ${system.work.size}, newWork ${system.newWork.size}")
        val conf = system.popWork()
        // no more work; reached fixed point
        if conf.isEmpty then
            val fpdg =
              if enableGraph then
                  system.bumps.foldLeft(dependencyGraph) { case (deps, (from, to)) =>
                    val n1 = asGraphElement(from, system)
                    val n2 = asGraphElement(to, system)

                    g.addEdge(deps, n1, BumpTransition(), n2)
                  }
              else dependencyGraph

            (system, fpdg)
        else
            // add an edge in the graph about the work
            val fdpg = if enableGraph && conf.get._1.nonEmpty then
                val n1 = asGraphElement(conf.get._1.get, system)
                val n2 = asGraphElement(conf.get._2, system)

                g.addEdge(dependencyGraph, n1, NoTransition(), n2)
            else dependencyGraph

            // candidate successors
            val successors = run(step(asState(conf.get._2, system)))
            decideSuccessors(fdpg, conf.get._2, successors, system)

trait SimpleWorklistSystem[E <: Expression] extends BaseSimpleWorklistSystem[E]:
    type System = SeenStateSystem
    override def inject(expr: Expr): System =
      new SeenStateSystem {}.pushWork(None, injectConf(expr))

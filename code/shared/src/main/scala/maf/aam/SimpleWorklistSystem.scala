package maf.aam

import maf.util.graph.*

trait BaseSimpleWorklistSystem extends AAMAnalysis:
    trait SeenStateSystem extends BaseSystem:
        this: System =>
        var seen: Set[Conf] = Set()
        var work: List[(Option[Conf], Conf)] = List()
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
          seen = seen + work
        }

        def allConfs: Set[Conf] = seen

        def finalStates: Set[State] =
          seen.map(asState(_, this)).filter(isFinal)

    type System <: SeenStateSystem

    protected def integrate(st: State, sys: System): (Conf, System) =
      (asConf(st, sys), sys)

    protected def decideSuccessors(prev: Conf, successors: Set[State], sys: System): System =
        successors.foreach { successor =>
            val (conf, sys1) = integrate(successor, sys)
            if !sys1.seen.contains(conf) then
                sys1.pushWork(Some(prev), conf)
                sys1.addSeen(conf)
        }

        sys

    override protected def transition[G](system: System, dependencyGraph: G)(using g: AAMGraph[G]): (System, G) =
        val conf = system.popWork()
        // no more work; reached fixed point
        if conf.isEmpty then (system, dependencyGraph)
        else
            // add an edge in the graph about the work
            val fdpg = if conf.get._1.nonEmpty then
                val n1 = asGraphElement(conf.get._1.get, system)
                val n2 = asGraphElement(conf.get._2, system)
                val dpg2 = g.addNode(dependencyGraph, n2)
                g.addEdge(dpg2, n1, NoTransition(), n2)
            else dependencyGraph

            // candidate successors
            val successors = step(asState(conf.get._2, system))
            val next = decideSuccessors(conf.get._2, successors, system)
            (next, fdpg)

trait SimpleWorklistSystem extends BaseSimpleWorklistSystem:
    type System = SeenStateSystem
    override def inject(expr: Expr): System =
      new SeenStateSystem {}.pushWork(None, injectConf(expr))

package maf.aam

trait BaseSimpleWorklistSystem extends AAMAnalysis:
    trait SeenStateSystem extends BaseSystem:
        this: System =>
        var seen: Set[Conf] = Set()
        var work: List[Conf] = List()
        var newWork: List[Conf] = List()

        def popWork(): Option[Conf] =
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
                work = newWork
                newWork = keep
                Some(conf)
              }
          else None

        def pushWork(work: Conf): this.type = change {
          newWork = work :: newWork
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

    protected def decideSuccessors(successors: Set[State], sys: System): System =
        successors.foreach { successor =>
            val (conf, sys1) = integrate(successor, sys)
            if !sys1.seen.contains(conf) then
                sys1.pushWork(conf)
                sys1.addSeen(conf)
        }

        sys

    override protected def transition(system: System): System =
        val conf = system.popWork()
        // no more work; reached fixed point
        if conf.isEmpty then system
        else
            // candidate successors
            val successors = step(asState(conf.get, system))
            decideSuccessors(successors, system)

trait SimpleWorklistSystem extends BaseSimpleWorklistSystem:
    type System = SeenStateSystem
    override def inject(expr: Expr): System =
      new SeenStateSystem {}.pushWork(injectConf(expr))

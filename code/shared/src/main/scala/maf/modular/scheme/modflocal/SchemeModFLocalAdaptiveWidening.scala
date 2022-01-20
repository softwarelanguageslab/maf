package maf.modular.scheme.modflocal

import maf.language.scheme._
import maf.modular.scheme._
import maf.modular.worklist._
import maf.util.benchmarks.Timeout
import maf.modular.AddrDependency
import maf.core._

trait SchemeModFLocalAdaptiveWidening extends SchemeModFLocal with SequentialWorklistAlgorithm[SchemeExp]:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    def debug(msg: => String) = ()

    // THE WIDENED SET DETERMINES WHICH ADDRESSES ARE WIDENED

    var widened: Set[Adr] = Set.empty
    override def customPolicy(adr: Adr): AddrPolicy =
      if widened(adr) then AddrPolicy.Widened else AddrPolicy.Local

    // BOOKKEEPING: keep track of all components per (lam, ctx)
    var cmps: Map[(Lam, Ctx), Set[Cll]] = Map.empty
    protected def onNewComponent(cll: Cll, cls: Set[Cll]): Unit = ()
    override def spawn(cmp: Cmp) =
        if (!visited(cmp)) then
            val cll @ CallComponent(lam, _, ctx, sto) = cmp
            val cls = cmps.getOrElse((lam, ctx), Set.empty) + cll
            cmps += (lam, ctx) -> cls
            debug(s"New component $cmp")
            onNewComponent(cll, cls)
        super.spawn(cmp)

    // THE SHADOW STORE (& DEPS)
    var shadowStore: Map[Adr, Val] = Map.empty
    var shadowDeps: Map[Adr, Set[Cmp]] = Map.empty

    // NOTE/TODO: not safe for parallelisation
    override protected def lookupLocal(cmp: Cmp, sto: Sto, adr: Adr): Option[(Val, Cnt)] =
        shadowDeps += adr -> (shadowDeps.getOrElse(adr, Set.empty) + cmp)
        super.lookupLocal(cmp, sto, adr)
    override protected def lookupLocal(cmp: Cmp, dlt: Dlt, adr: Adr): Option[(Val, Cnt)] =
        shadowDeps += adr -> (shadowDeps.getOrElse(adr, Set.empty) + cmp)
        super.lookupLocal(cmp, dlt, adr)
    override protected def extendLocal(cmp: Cmp, sto: Sto, adr: Adr, vlu: Val): Dlt =
        updateAddr(shadowStore, adr, vlu).foreach(upd => shadowStore = upd)
        super.extendLocal(cmp, sto, adr, vlu)
    override protected def updateLocal(cmp: Cmp, sto: Sto, adr: Adr, vlu: Val): Dlt =
        updateAddr(shadowStore, adr, vlu).foreach(upd => shadowStore = upd)
        super.updateLocal(cmp, sto, adr, vlu)

    protected def addWidened(wid: Set[Adr]) =
        // helper functions
        def widenSto(sto: Sto): Sto = sto -- wid
        def widenDlt(dlt: Dlt): Dlt = dlt -- wid
        def widenCll(cll: Cll): Cll = cll.copy(sto = widenSto(cll.sto))
        def widenCmp(cmp: Cmp): Cmp =
          cmp match
              case MainComponent => cmp
              case cll: Cll      => widenCll(cll)
        def widenDep(dep: Dep): Dep =
          dep match
              case res: ResultDependency => res.copy(cmp = widenCmp(res.cmp))
              case _                     => dep
        // add widened addresses
        widened ++= wid
        // update analysis data
        var toTrigger: Set[Dep] = Set.empty //TODO: this only works when forall cmp: |deps(ResultDependency(cmp))| > 0
        visited = visited.map(widenCmp)
        workList = workList.map(widenCmp)
        deps = deps
          .foldLeft(Map.empty[Dep, Set[Cmp]]) { case (acc, (dep, cps)) =>
            val updatedCps = cps.map(widenCmp)
            val updatedDep = widenDep(dep)
            acc.get(updatedDep) match
                case None => acc + (updatedDep -> updatedCps)
                case Some(oth) =>
                  toTrigger += updatedDep
                  acc + (updatedDep -> (oth ++ updatedCps))
          }
          .withDefaultValue(Set.empty)
        lcls = lcls.map((adr, rfs) => (adr, rfs -- wid))
        cmps = cmps.map((nod, cls) => (nod, cls.map(widenCll)))
        shadowDeps = shadowDeps.map((adr, cps) => (adr, cps.map(widenCmp)))
        // widen addresses (using shadow store & deps)
        widened.foreach { adr =>
            writeAddr(adr, shadowStore.getOrElse(adr, lattice.bottom))
            addToWorkList(shadowDeps.getOrElse(adr, Set.empty))
            shadowStore -= adr
            shadowDeps -= adr
        }
        // update results
        results = results.foldLeft(Map.empty: Res) { case (acc, (cmp, (vlu, dlt))) =>
          val updatedCmp = widenCmp(cmp)
          val updatedDlt = widenDlt(dlt)
          acc.get(updatedCmp) match
              case None =>
                acc + (updatedCmp -> (vlu, updatedDlt))
              case Some((othVlu, othDlt)) =>
                acc + (updatedCmp -> (lattice.join(othVlu, vlu), updatedCmp.sto.join(othDlt, updatedDlt)))
        }
        // trigger "merged" dependencies
        toTrigger.foreach(trigger)

    // HELPERS

    protected def pickAddrs(sts: Set[Sto], cut: Int): Set[Adr] =
        val kys = sts.flatMap(_.content.keySet)
        val ads = sts.foldLeft(Map.empty[Adr, Set[(Val, Cnt)]]) { (acc, sto) =>
          kys.foldLeft(acc) { case (acc2, adr) =>
            val bnd = sto.content.getOrElse(adr, (lattice.bottom, CountZero))
            acc2.get(adr) match
                case None      => acc2 + (adr -> Set(bnd))
                case Some(bds) => acc2 + (adr -> (bds + bnd))
          }
        }
        val lst = ads.toList.sortBy((adr, bds) => bds.size)(Ordering[Int].reverse)
        pickAddrsRec(lst, sts, cut)

    private def pickAddrsRec(lst: List[(Adr, Set[(Val, Cnt)])], sts: Set[Sto], cut: Int): Set[Adr] =
      if sts.size > cut then
          val (adr, _) :: rst = lst
          pickAddrsRec(rst, sts.map(_ - adr), cut) + adr
      else Set.empty

// DEBUGGING CODE

/*
    private def checkForChanges(cmp: Cmp) =
      val anl = new SchemeLocalIntraAnalysis(cmp)
      anl.analyzeWithTimeout(Timeout.none)
      assert(anl.C.filterNot(visited).isEmpty, {
        val dsc = anl.C.filterNot(visited).head
        val sto0 = cmp.sto
        val sto1 = dsc.sto
        val diff = sto1.content.toSet -- sto0.content.toSet
        diff.foreach { (adr, vlu) =>
          println(s"$adr -> $vlu (was: ${sto0(adr)})")
        }
      })

    private def checkWorklist() =
      (visited -- workList.toList).foreach(checkForChanges)
 */

//
// POLICY A
//

trait SchemeModFLocalAdaptiveWideningPolicyA(n: Int, c: Double = 0.5) extends SchemeModFLocalAdaptiveWidening:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    val cut = Math.max(n * c, 1).toInt

    private var toAdapt: Set[(Lam, Ctx)] = Set.empty
    override def onNewComponent(cll: Cll, cls: Set[Cll]) =
        super.onNewComponent(cll, cls)
        if cls.size > n then toAdapt += (cll.lam, cll.ctx)

    override def step(t: Timeout.T) =
        super.step(t)
        if toAdapt.nonEmpty then
            val wid = toAdapt.flatMap { (lam, ctx) =>
                val cps = cmps((lam, ctx))
                val sts = cps.map(_.sto)
                pickAddrs(sts, cut)
            }
            addWidened(wid)
            debug(s"=> Widened ${wid.size} addresses (total: ${widened.size})")
            toAdapt = Set.empty

//
// POLICY B
//

trait SchemeModFLocalAdaptiveWideningPolicyB(l: Int, c: Double = 0.5) extends SchemeModFLocalAdaptiveWidening:
    this: SchemeModFLocalSensitivity with SchemeDomain =>

    def ratio: Double = (visited.size - 1) / Math.max(1, cmps.size)

    override def step(t: Timeout.T) =
        super.step(t)
        if ratio > l then
            val oldRatio = ratio
            adaptAnalysis()
            debug(s"=> ${widened.size} addresses have been widened in total.")
            val newRatio = ratio
            debug(s"Ratio: $oldRatio -> $newRatio")

    private def adaptAnalysis() =
        val max = cmps.maxBy((_, cls) => cls.size)._2.size
        val cut = Math.max(1, max * c).toInt
        val sel = cmps.filter((_, cls) => cls.size >= cut)
        val wid = sel.flatMap((_, cls) => pickAddrs(cls.map(_.sto), cut)).toSet
        addWidened(wid)

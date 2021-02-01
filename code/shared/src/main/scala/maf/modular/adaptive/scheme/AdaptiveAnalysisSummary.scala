package maf.modular.adaptive.scheme

import maf.language.scheme._
import maf.modular.scheme.modf._
import maf.modular.adaptive.scheme._
import maf.util.MonoidImplicits._
import maf.util.datastructures._
import maf.modular._
import maf.util.MonoidInstances

trait AdaptiveAnalysisSummary extends AdaptiveSchemeModFSemantics {

  trait SchemeModule
  case object MainModule extends SchemeModule
  case class LambdaModule(fun: SchemeLambdaExp) extends SchemeModule

  def module(cmp: Component): SchemeModule = view(cmp) match {
    case Main       => MainModule
    case call: Call => LambdaModule(call.clo._1)
  }

  /**
   * Summarizes analysis information by:
   * - keeping track of a `ModuleSummary` for each module
   * - keeping track, for each dependency, which modules have components (at least one) that have been triggered (at least once) by that dependency
   */
  case class AnalysisSummary(content: Map[SchemeModule, ModuleSummary], depFns: Map[Dependency, Set[SchemeModule]]) {
    def apply(fn: SchemeModule) = content(fn)
    def get(fn: SchemeModule) =
      content.getOrElse(fn, ModuleSummary.empty)
    def addComponent(cmp: Component): AnalysisSummary =
      addComponent(module(cmp), cmp)
    def addComponent(fun: SchemeModule, cmp: Component): AnalysisSummary = {
      val prv = this.get(fun)
      val upd = prv.addComponent(cmp)
      onCostIncrease(fun, upd.cost)
      AnalysisSummary(content + (fun -> upd), depFns)
    }
    def addDependency(cmp: Component, dep: Dependency): AnalysisSummary =
      addDependency(module(cmp), cmp, dep)
    def addDependency(
        fun: SchemeModule,
        cmp: Component,
        dep: Dependency
      ): AnalysisSummary = {
      // update content
      val prv = content(fun) // guaranteed to have a module summary already!
      val upd = prv.addDependency(cmp, dep)
      // update depFns
      val fns = depFns.getOrElse(dep, Set.empty) + fun
      onCostIncrease(fun, upd.cost)
      AnalysisSummary(content + (fun -> upd), depFns + (dep -> fns))
    }
    def clearDependency(dep: Dependency) = {
      val fns = depFns(dep)
      val upd = fns.foldLeft(content) { (acc, fun) =>
        acc + (fun -> acc(fun).clearDependency(dep))
      }
      AnalysisSummary(upd, depFns - dep)
    }
  }

  object AnalysisSummary {
    def empty = AnalysisSummary(Map.empty, Map.empty)
  }

  /**
   * Summarizes module information by:
   * - keeping track of all components of that module
   * - keeping track, for each component of that module, how many times a dependency triggered a re-analysis of that component
   * - keeping track, for each dependency, which components were triggered (at least once) by that dependency
   * - keeping track the total number of times a component of this module was triggered by some dependency
   */
  case class ModuleSummary(
      content: Map[Component, MultiSet[Dependency]],
      depCmps: Map[Dependency, Set[Component]],
      totalDepCount: Int) {
    def cost = numberOfCmps + totalDepCount // total cost = number of components + number of re-analyses
    def components = content.keys
    def numberOfCmps = content.size
    def depCounts = content.values.reduce(_ ++ _) // only safe if cost > 0
    def addComponent(cmp: Component) =
      ModuleSummary(content + (cmp -> MultiSet.empty), depCmps, totalDepCount)
    def addDependency(cmp: Component, dep: Dependency) =
      ModuleSummary(content + (cmp -> (content(cmp) + dep)), depCmps + (dep -> (depCmps.getOrElse(dep, Set.empty) + cmp)), totalDepCount + 1)
    def clearDependency(dep: Dependency) = {
      val cps = depCmps(dep)
      val (upd, cnt) = cps.foldLeft((content, totalDepCount)) { (acc, cmp) =>
        val prv = acc._1(cmp)
        val cnt = prv.getMult(dep)
        val upd = prv.removeAll(dep)
        (acc._1 + (cmp -> upd), acc._2 - cnt)
      }
      ModuleSummary(upd, depCmps - dep, cnt)
    }
  }

  object ModuleSummary {
    def empty = ModuleSummary(Map.empty, Map.empty, 0)
  }

  private def updateAnalysisSummary(update: Component => Component)(as: AnalysisSummary): AnalysisSummary =
    AnalysisSummary(updateMap(updateModuleSummary(update))(as.content), updateMap(updateDep(update), (s: Set[SchemeModule]) => s)(as.depFns))
  private def updateModuleSummary(update: Component => Component)(ms: ModuleSummary): ModuleSummary = {
    val updated = updateMap(update, updateMultiSet(updateDep(update), Math.max))(ms.content)(MonoidInstances.multiSetMaxMonoid)
    ModuleSummary(updated, updateMap(updateDep(update), updateSet(update))(ms.depCmps), updated.values.map(_.cardinality).sum)
  }

  // keep track of a summary for the current analysis
  var summary: AnalysisSummary = AnalysisSummary.empty.addComponent(initialComponent)
  // allow to detect when the cost of a given module increases
  def onCostIncrease(fn: SchemeModule, newCost: Int)
  // update the summary each time a new component is discovered
  override def onNewComponent(cmp: Component, call: Call) =
    summary = summary.addComponent(LambdaModule(call.clo._1), cmp)
  // update the summary each time a dependency triggers a component
  override def trigger(dep: Dependency): Unit = {
    deps.getOrElse(dep, Set.empty).foreach { cmp =>
      summary = summary.addDependency(cmp, dep)
    }
    super.trigger(dep)
  }
  // correctly update the summary after adaptation
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.summary = updateAnalysisSummary(update)(summary)
  }
}

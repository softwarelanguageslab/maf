package maf.modular.adaptive

import maf.modular.components.IndirectComponents
import maf.core._
import maf.modular._
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util._
import maf.util.datastructures._
import maf.util.MonoidImplicits._
import maf.util.benchmarks.Timeout

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr)
    extends ModAnalysis(program)
       with IndirectComponents[Expr]
       with SequentialWorklistAlgorithm[Expr]
       with DependencyTracking[Expr] {

  import maf.modular.components.IndirectComponents._

  var mainComponent: Component = _

  // after every step, the adaptive analysis gets an opportunity to reflect on (introspection) and possible change (intercession) the analysis behaviour
  // the method `adaptAnalysis` needs to be implemented to decide when and how this is carried out
  protected def adaptAnalysis(): Unit
  override def step(timeout: Timeout.T): Unit = {
    super.step(timeout)
    adaptAnalysis()
  }
  // the core of the adaptive analysis: one needs to implement how components are to be "adapted"
  // the idea is that the definition/results of `adaptComponent` can change during the analysis itself ...
  def adaptComponent(cmp: ComponentData): ComponentData
  // .. and that when this happens, one needs to call `updateAnalysis`
  def updateAnalysis() = {
    // update the indirection maps and calculate the "new component pointer" for every "old component pointer"
    val moved = updateComponentMapping()
    // update all components pointers in the analysis
    // println(moved)
    moved.keys.foreach(dealloc)
    val lifted = moved.map(p => (ComponentPointer(p._1), ComponentPointer(p._2)))
    updateAnalysisData(lifted.withDefault(ptr => ptr))
  }

  private def updateComponentMapping() = {
    // bookkeeping to keep track of "joined"/moved component pointers
    val ds = new DisjointSet[Address]()
    var destinations = Map.empty[Address, Address]
    var moved = Set.empty[Address]
    def updateAddr(addr: Address): Address =
      if (moved.contains(addr)) {
        destinations(ds.find(addr))
      } else {
        addr
      }
    // the actual fixed-point computation
    var update: ComponentData => ComponentData = adaptComponent
    var dirty = true
    while(dirty) {
      dirty = false
      val previous = this.cMapR
      this.cMapR = Map.empty[ComponentData, Address]
      // compute updated components
      previous.foreach { case (oldCmp, oldAddr) =>
        val newCmp = update(oldCmp)
        this.cMapR.get(newCmp) match {
          case None => this.cMapR += newCmp -> oldAddr
          case Some(newAddr) =>
            val parent = ds.merge(newAddr, oldAddr)
            destinations += parent -> newAddr
            moved += oldAddr
            dirty = true
        }
      }
      update = updateCmp(ptr => ComponentPointer(updateAddr(ptr.addr)))
    }
    // compute cMap
    this.cMap = this.cMapR.map(_.swap)
    // return a map representing all moved addresses
    moved.map(addr => (addr, updateAddr(addr))).toMap
  }

  // ... which in turn calls `updateAnalysisData` to update the component pointers
  def updateAnalysisData(update: Map[Component, Component]) = {
    workList = workList.map(update)
    visited = updateSet(update)(visited)
    newComponents = updateSet(update)(newComponents)
    dependencies = updateMap(update, updateSet(update))(dependencies)
    deps = updateMap(updateDep(update), updateSet(update))(deps)
    mainComponent = update(mainComponent)
  }
  // the analysis' data structures need to be updated after adaptation, as some components may now be equal
  // the following methods need to be implemented by a subclass, since they depend on the representation of 'ComponentData' and 'Dependency'
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData
  def updateDep(update: Component => Component)(dep: Dependency): Dependency = dep
  // the following methods are convenience methods to update standard compound data structures
  def updateSet[V](update: V => V)(set: Set[V]): Set[V] = set.map(update)
  def updateMap[K, V](update: V => V)(map: Map[K, V]): Map[K, V] = map.view.mapValues(update).toMap
  def updateMap[K, V: Monoid](updateK: K => K, updateV: V => V)(map: Map[K, V]): Map[K, V] =
    map.foldLeft(Map[K, V]().withDefaultValue(Monoid[V].zero)) { case (acc, (key, vlu)) =>
      val keyAbs = updateK(key)
      acc + (keyAbs -> Monoid[V].append(acc(keyAbs), updateV(vlu)))
    }
  def updatePair[P, Q](updateA: P => P, updateB: Q => Q)(p: (P, Q)): (P, Q) = (updateA(p._1), updateB(p._2))
  def updateMultiSet[X](update: X => X, intOp: (Int, Int) => Int)(ms: MultiSet[X]): MultiSet[X] =
    ms.toMap.foldLeft(MultiSet.empty[X]) { case (acc, (elm, count)) =>
      val elmAbs = update(elm)
      acc.updateMult(elmAbs) {
        case 0 => count
        case n => intOp(n, count)
      }
    }

  private var free: List[Address] = _
  override protected def alloc() =
    if (free.isEmpty) {
      super.alloc()
    } else {
      val first = free.head
      free = free.tail
      first
    }

  private def dealloc(addr: Address) =
    free = addr :: free

  override def init() = {
    free = Nil
    super.init()
    mainComponent = initialComponent
  }
}

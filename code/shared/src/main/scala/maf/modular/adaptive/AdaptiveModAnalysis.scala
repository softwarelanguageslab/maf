package maf.modular.adaptive

import maf.core._
import maf.modular._
import maf.modular.worklist._
import maf.util._
import maf.util.datastructures._
import maf.util.MonoidImplicits._
import maf.util.benchmarks.Timeout

abstract class AdaptiveModAnalysis[Expr <: Expression](program: Expr, val rate: Int = 1000)
    extends ModAnalysis(program)
    with SequentialWorklistAlgorithm[Expr]:

    // after every `rate` steps, the adaptive analysis gets an opportunity to reflect on (introspection) and possible change (intercession) the analysis behaviour
    // the method `inspect` needs to be implemented to decide when and how this is carried out
    protected def inspect(): Unit

    protected var stepCount = 0
    override def step(timeout: Timeout.T): Unit =
        stepCount += 1
        super.step(timeout)
        if stepCount == rate then
            stepCount = 0
            inspect()

    // the core of the adaptive analysis: one needs to implement how components are to be "adapted"
    // the idea is that the definition/results of `adaptComponent` can change during the analysis itself ...
    def adaptComponent(cmp: Component): Component

    // .. and that when this happens, one needs to call `updateAnalysis` to update all analysis data
    def adaptAnalysis() =
        workList = workList.map(adaptComponent)
        visited = adaptSet(adaptComponent)(visited)
        deps = adaptMap(adaptDep, adaptSet(adaptComponent))(deps)
    // the analysis' data structures need to be updated after adaptation
    // the following method needs to be implemented by a subclass, since it depends on the representation of 'Dependency'
    def adaptDep(dep: Dependency): Dependency = dep
    // the following methods are convenience methods to update standard compound data structures
    def adaptSet[V](adapt: V => V)(set: Set[V]): Set[V] = set.map(adapt)
    def adaptMap[K, V](adapt: V => V)(map: Map[K, V]): Map[K, V] = map.view.mapValues(adapt).toMap
    def adaptMap[K, V: Monoid](adaptK: K => K, adaptV: V => V)(map: Map[K, V]): Map[K, V] =
      map.foldLeft(Map[K, V]().withDefaultValue(Monoid[V].zero)) { case (acc, (key, vlu)) =>
        val keyAbs = adaptK(key)
        acc + (keyAbs -> Monoid[V].append(acc(keyAbs), adaptV(vlu)))
      }
    def adaptPair[P, Q](adaptA: P => P, adaptB: Q => Q)(p: (P, Q)): (P, Q) = (adaptA(p._1), adaptB(p._2))
    def adaptMultiSet[X](adaptX: X => X, intOp: (Int, Int) => Int)(ms: MultiSet[X]): MultiSet[X] =
      ms.toMap.foldLeft(MultiSet.empty[X]) { case (acc, (elm, count)) =>
        val elmAbs = adaptX(elm)
        acc.updateMult(elmAbs) {
          case 0 => count
          case n => intOp(n, count)
        }
      }

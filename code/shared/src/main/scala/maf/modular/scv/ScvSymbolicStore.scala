package maf.modular.scv

import maf.modular.{AbstractDomain, Dependency, ModAnalysis}
import maf.lattice.interfaces.BoolLattice
import maf.language.symbolic.lattices.SymbolicSchemeDomain
import maf.core.{Expression, Lattice, LatticeTopUndefined}
import maf.util.datastructures.ListOps.Crossable
import maf.util.{Monoid, StringUtil}
import java.awt.Component

/**
 * A generic global store, which is not constrained on the type of values it can store, and is only concerned with values that can be appended
 * together (i.e., implement Monoid). This generic global store interface also features a way to forcefully update a particular value.
 *
 * @tparam V
 *   the type of values to store in the store
 * @tparam Exp
 *   the type of expression used in the analysis
 */
trait GlobalMapStore[Exp <: Expression] extends ModAnalysis[Exp] { inter =>
    type V
    type A

    given vMonoid: Monoid[V]

    private var store: Map[A, V] = Map()
    override def intraAnalysis(cmp: Component): GlobalMapStoreIntra

    case class AddrMapDependency(addr: A) extends Dependency

    /** Forcefully write the `newvalue` to the specific `addr` in the store */
    private def writeAddr(addr: A, newvalue: V): Boolean =
        inter.store.get(addr) match
            case Some(oldvalue) if oldvalue == newvalue => false
            case _ =>
                store = store + (addr -> newvalue)
                true

    private def updateAddr[V: Monoid](
        store: Map[A, V],
        addr: A,
        value: V
      ): Option[Map[A, V]] =
        store.get(addr) match
            case None if Monoid[V].zero == value => None
            case None                            => Some(store + (addr -> value))
            case Some(oldValue) =>
                val newValue = Monoid[V].append(oldValue, value)
                if newValue == oldValue then None
                else
                    //println(s"joining $value with $oldValue")
                    Some(store + (addr -> newValue))

    trait GlobalMapStoreIntra extends IntraAnalysis { intra =>
        private var store = inter.store

        def writeMapAddr(addr: A, value: V): Unit =
            updateAddr[V](intra.store, addr, value).map { updated =>
                intra.store = updated
                trigger(AddrMapDependency(addr))
            }.isDefined

        def writeMapAddrForce(addr: A, value: V): Unit =
            intra.store += (addr -> value)
            trigger(AddrMapDependency(addr))

        def readMapAddr(addr: A): V =
            register(AddrMapDependency(addr))
            intra.store.getOrElse(addr, Monoid[V].zero)

        override def doWrite(dep: Dependency): Boolean = dep match
            case AddrMapDependency(addr) => inter.writeAddr(addr, intra.store(addr))
            case _                       => super.doWrite(dep)
    }

    def mapStoreString(primitives: Boolean = false): String =
        val strings = store.map({ case (a, v) => s"${StringUtil.toWidth(a.toString, 50)}: $v" })
        val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
        val size = filtered.size
        val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
        filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)
}

object ScvSymbolicStore:
    import maf.language.scheme._
    import maf.language.symbolic.*

    trait GlobalSymbolicStore extends GlobalMapStore[SchemeExp]:
        type V = Formula
        type A = Component

        given vMonoid: Monoid[Formula] with
            import FormulaAux.*
            def zero: Formula = EmptyFormula
            def append(x: Formula, y: => Formula): Formula = DNF.dnf(disj(x, y))

object ScvPathSensitiveSymbolicStore:
    import maf.language.scheme._
    import maf.language.symbolic.*

    trait GlobalPathSensitiveSymbolicStore extends GlobalMapStore[SchemeExp] with AbstractDomain[SchemeExp] with SymbolicSchemeDomain:
        type V = Map[Formula, Value]
        type A = Component

        def subsumes(x: Formula, y: Formula): Boolean = (x, y) match
            case (Conjunction(xs), Conjunction(ys)) => xs subsetOf ys
            case (_, _)                             =>
                // TODO: make this more precise by finding an order to works on more than conjunctions
                false

        def joinValue(x: Value, y: Value): Value =
            lattice.setRight(lattice.join(x, y), Set())

        given vMonoid: Monoid[V] with
            def zero: V = Map()
            def append(x: V, y: => V): V =
                x.toList
                    .cartesian(y.toList)
                    .flatMap { case ((p1, (v1)), (p2, (v2))) =>
                        // path conditions are equal
                        if subsumes(p2, p1) && subsumes(p1, p2) then
                            // we keep p1 (p2 would be equivalent) and join the value
                            List((p1 -> (joinValue(v1, v2))))
                        else if subsumes(p2, p1) && !subsumes(p1, p2) then
                            // p2 is more generic then p1 so we keep p2 and throw away p1
                            List((p2 -> (joinValue(v1, v2))))
                        else if !subsumes(p2, p1) && subsumes(p1, p2) then
                            // p1 is more generic then p2 we keep p1 and throw away p2
                            List((p1 -> (joinValue(v1, v2))))
                        else
                            // p1 and p2 do not have any relation with each other, we keep them both in the map
                            List((p1 -> joinValue(v1, lattice.bottom)), (p2 -> joinValue(v2, lattice.bottom)))
                    }
                    .toMap

//x.keys.foreach { key =>
//    if Some(x(key)) != result.get(key) then println(s"difference spotted at $key\n ==== x: ${x(key)}\n ==== y ${result.get(key)}")
//}
//println(s"${x.keys.size} and result size ${result.keys.size}")
//println("====")
//
//(x.keys ++ y.keys).foldLeft(x)((result, key) =>
//    result + (key -> lattice.join(x.get(key).getOrElse(lattice.bottom), y.get(key).getOrElse(lattice.bottom)))
//)

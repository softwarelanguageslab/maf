package maf.aam.scheme.stores

import maf.aam.scheme.*

import maf.aam.*
import maf.core.*
import maf.util.*

/**
 * A local store version of the Scheme AAM-style semantics. The local store is threaded through the analysis states, and is never joined with other
 * stores. This enables maximal precision but induces a large performance penalty.
 *
 * This version of the AAM semantics is the closest to the classic AAM paper by Van Horn et al. in 2010.
 */
trait SchemeAAMLocalStore extends BaseSchemeAAMSemantics:
    type Sto = BasicStore[Address, Storable]
    type Conf = State

    override protected def asState(conf: Conf, sys: System): State = conf
    override protected def asConf(state: State, sys: System): Conf = state

    override lazy val initialStore: Sto = BasicStore(initialBds.map(p => (p._2, p._3)).toMap).extend(Kont0Addr, Storable.K(Set(HltFrame)))

    def readSto(sto: Sto, addr: Address): (Storable, Sto) =
      (sto.lookup(addr).getOrElse(Storable.V(lattice.bottom)), sto)

    /** Write to the given address in the store, returns the updated store */
    override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
      sto.extend(addr, value)

    override def compareStates(s1: Conf, s2: Conf): Boolean =
        println("==================================================================================")
        for (k, v) <- s1.s.content do
            if s2.s.content.contains(k) && v != s2.s.content(k) then println(s"Difference detected at $k of $v and ${s2.s.content(k)}")
        println("==================================================================================")
        if s1.k != s2.k then println(s"**** difference in cnts ${s1.k} <======> ${s2.k}")
        true

    override def storeString(store: Sto, primitives: Boolean = true): String =
        val strings = store.content.map({ case (a, v) => s"${StringUtil.toWidth(a.toString, 50)}: $v" })
        val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
        val size = filtered.size
        val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
        filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)

    override def printDebug(s: Conf, printStore: Boolean = false): Unit =
        println(s)
        if printStore then println(storeString(s.s, false))

    /** Inject the initial state for the given expression */
    def injectConf(expr: Expr): Conf =
      SchemeState(Control.Ev(expr, initialEnv), initialStore, Kont0Addr, initialTime, emptyExt)

    override def asGraphElement(c: Conf, sys: System): GraphElementAAM =
      asGraphElement(c.c, c.k, c.s, c.extra, c.hashCode)

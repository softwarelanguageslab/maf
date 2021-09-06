package maf.modular.scheme

import maf.core._
import maf.language.CScheme._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.modular._
import maf.modular.scheme._

/**
 * Prepares a modular analysis to analyse Scheme programs: <ul> <li> Ensures the program is undefined and lexically addressed. <li> Provides an
 * initial lexical environment for the analysis. <li> Sets up a global store, containing bindings for the language primitives in the provided
 * environment. </ul>
 */
trait SchemeSetup
    extends ModAnalysis[SchemeExp]
    with GlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SchemeDomain
    with AnalysisResults[SchemeExp]:
    // Provide a global store
    override var store: Map[Addr, Value] = Map.empty
    // collect result from variable and pointer addresses
    def resultsPerIdn: Map[Identity, Set[Value]] =
      store
        .filter(_._1 match {
          case _: VarAddr[_] | _: PtrAddr[_] => true
          case _                             => false
        })
        .groupBy(_._1.idn)
        .view
        .mapValues(_.values.toSet)
        .toMap
    final lazy val initialBds: Iterable[(String, Addr, Value)] = primitives.allPrimitives.map { case (name, p) =>
      (name, PrmAddr(name), lattice.primitive(p.name))
    }
    final lazy val initialEnv: Environment[Addr] = Environment(initialBds.map(bnd => (bnd._1, bnd._2)))
    // Set up initial environment and install the primitives in the global store.
    override def init() =
        super.init()
        initialBds.foreach(bnd => store += bnd._2 -> bnd._3)

package maf.modular.scheme

import maf.core._
import maf.language.CScheme._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.modular._
import maf.modular.scheme._

/**
 * Prepares a modular analysis to analyse Scheme programs:
 * <ul>
 *   <li> Ensures the program is undefined and lexically addressed.
 *   <li> Provides an initial lexical environment for the analysis.
 *   <li> Sets up a global store, containing bindings for the language primitives in the provided environment.
 * </ul>
 */
trait SchemeSetup extends ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] with SchemeDomain {
  // Provide a global store
  override var store: Map[Addr, Value] = Map.empty
  // Ensure that the program is translated to use lexical addresses first!
  override def program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    CSchemeUndefiner.undefine(List(preludedProgram))
  }
  lazy val initialBds: Iterable[(String, Addr, Value)] = primitives.allPrimitives.map { case (name, p) =>
    (name, PrmAddr(name), lattice.primitive(p.name))
  }
  lazy val initialEnv: Environment[Addr] = Environment(initialBds.map(bnd => (bnd._1, bnd._2)))
  // Set up initial environment and install the primitives in the global store.
  initialBds.foreach(bnd => store += bnd._2 -> bnd._3)
}

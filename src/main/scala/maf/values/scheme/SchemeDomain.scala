package maf.values
package scheme

trait SchemeDomain:
  type Val
  given lattice: SchemeLattice[Val]

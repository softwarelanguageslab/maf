package maf.modular.incremental

/**
 * Utility class to select the optimisations of the incremental analysis that should be enabled.
 *
 * @param componentInvalidation
 *   Indicates whether components should be invalidated when they are no longer spawned. This involves a possibly recursive traversal of spawn and
 *   cycle detection. This includes the deletion of the corresponding return addresses.
 * @param dependencyInvalidation
 *   Indicates whether (read) dependencies should be removed when they are no longer inferred.
 * @param writeInvalidation
 *   Indicates whether writes should be refined using provenance tracking. This includes the removal of addresses that are no longer written.
 * @param cyclicValueInvalidation
 *   Indicates whether writeInvalidation should be improved upon by tracking which values depend on each other, and by invalidating (lowering) values
 *   at addresses in a SCA.
 */
case class IncrementalConfiguration(
    componentInvalidation: Boolean = true,
    dependencyInvalidation: Boolean = true,
    writeInvalidation: Boolean = true,
    cyclicValueInvalidation: Boolean = true) {

  if (cyclicValueInvalidation && !writeInvalidation)
    throw new Exception("Illegal configuration state: cyclic value invalidation requires write invalidation.")

  private def booleanToString(b: Boolean): String = if (b) "enabled" else "disabled"

  def infoString(): String =
    s"""****** Incremental configuration ******
       | + Component invalidation: ${booleanToString(componentInvalidation)}
       | + Dependency invalidation: ${booleanToString(dependencyInvalidation)}
       | + Write invalidation: ${booleanToString(writeInvalidation)}
       |    - Cycle detection: ${booleanToString(cyclicValueInvalidation)}
       |***************************************""".stripMargin

  override def toString: String = {
    val ci = if (componentInvalidation) "CI" else ""
    val di = if (dependencyInvalidation) "DI" else ""
    val wi = if (writeInvalidation) {
      if (cyclicValueInvalidation) "WI+CY" else "WI"
    } else ""
    val string = List(ci, di, wi).filterNot(_.isEmpty).mkString("-")
    if (string.isEmpty) "NoOpt" else string
  }
}

/** Provides instantiations of all configurations for the incremental analysis. */
object IncrementalConfiguration {

  // For simplicity, already instantiate all possible configurations here so they can be referred by name.

  lazy val noOptimisations: IncrementalConfiguration = IncrementalConfiguration(false, false, false, false)

  lazy val ci: IncrementalConfiguration = IncrementalConfiguration(true, false, false, false)
  lazy val di: IncrementalConfiguration = IncrementalConfiguration(false, true, false, false)
  lazy val wi: IncrementalConfiguration = IncrementalConfiguration(false, false, true, false)
  lazy val wi_cy: IncrementalConfiguration = IncrementalConfiguration(false, false)

  lazy val ci_di: IncrementalConfiguration = IncrementalConfiguration(true, true, false, false)
  lazy val ci_wi: IncrementalConfiguration = IncrementalConfiguration(true, false, true, false)
  lazy val di_wi: IncrementalConfiguration = IncrementalConfiguration(false, true, true, false)

  lazy val ci_di_wi: IncrementalConfiguration = IncrementalConfiguration(true, true, true, false)
  lazy val ci_wi_cy: IncrementalConfiguration = IncrementalConfiguration(true, false)
  lazy val di_wi_cy: IncrementalConfiguration = IncrementalConfiguration(false)

  lazy val allOptimisations: IncrementalConfiguration = IncrementalConfiguration() // ci_di_wi_cy

  /** A list of all possible configurations for the incremental analysis (CY requires WI). */
  lazy val allConfigurations: List[IncrementalConfiguration] =
    List(
      // No optimisations
      noOptimisations,
      // One optimisation
      ci,
      di
      //wi,
      // Two optimisations
      //ci_di,
      //ci_wi,
      //di_wi,
      //wi_cy,
      // Three optimisations
      //ci_di_wi
      //ci_wi_cy,
      //di_wi_cy,
      // Four optimisations
      //allOptimisations,
    )
}

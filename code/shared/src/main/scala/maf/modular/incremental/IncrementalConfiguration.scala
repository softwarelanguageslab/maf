package maf.modular.incremental

import maf.modular.incremental.IncrementalConfiguration.InvalidConfigurationException

/**
 * Utility class to select the optimisations of the incremental analysis that should be enabled. This can be specified using the boolean arguments to
 * the constructor of the class. Additionally, other analysis specifications can be specified as well. These can be defined using `var` fields (or as
 * `def`s).
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
    cyclicValueInvalidation: Boolean = true):

    if cyclicValueInvalidation && !writeInvalidation then
        throw new InvalidConfigurationException("Illegal configuration state: cyclic value invalidation requires write invalidation.", this)

    private def booleanToString(b: Boolean): String = if b then "enabled" else "disabled"

    def infoString(): String =
      s"""****** Incremental configuration ******
       | + Component invalidation: ${booleanToString(componentInvalidation)}
       | + Dependency invalidation: ${booleanToString(dependencyInvalidation)}
       | + Write invalidation: ${booleanToString(writeInvalidation)}
       |    - Cycle detection: ${booleanToString(cyclicValueInvalidation)}
       |***************************************""".stripMargin

    override def toString: String =
        val ci = if componentInvalidation then "CI" else ""
        val di = if dependencyInvalidation then "DI" else ""
        val wi =
          if writeInvalidation then if cyclicValueInvalidation then "WI+CY" else "WI"
          else ""
        val string = List(ci, di, wi).filterNot(_.isEmpty).mkString("-")
        if string.isEmpty then "NoOpt" else string

    // Utility to determine between fast/slow tests based on the number of active optimisations.
    def rank(): Int =
        val ci = if componentInvalidation then 1 else 0
        val di = if dependencyInvalidation then 1 else 0
        val wi = if writeInvalidation then 1 else 0
        val cy = if cyclicValueInvalidation then 1 else 0
        ci + di + wi + cy

    /**
     * * Other analysis specifications **
     */

    /** Specifies whether assertions should be checked. This flag can be disabled for performance benchmarks. */
    var checkAsserts: Boolean = true

    def disableAsserts(): IncrementalConfiguration =
        val copy = this.copy()
        copy.checkAsserts = false
        copy

/** Provides instantiations of all configurations for the incremental analysis. */
object IncrementalConfiguration:

    // For simplicity, already instantiate all possible configurations here so they can be referred by name.

    lazy val noOptimisations: IncrementalConfiguration = IncrementalConfiguration(false, false, false, false)

    lazy val ci: IncrementalConfiguration = IncrementalConfiguration(true, false, false, false)
    lazy val di: IncrementalConfiguration = IncrementalConfiguration(false, true, false, false)
    lazy val wi: IncrementalConfiguration = IncrementalConfiguration(false, false, true, false)

    lazy val ci_di: IncrementalConfiguration = IncrementalConfiguration(true, true, false, false)
    lazy val ci_wi: IncrementalConfiguration = IncrementalConfiguration(true, false, true, false)
    lazy val di_wi: IncrementalConfiguration = IncrementalConfiguration(false, true, true, false)
    lazy val wi_cy: IncrementalConfiguration = IncrementalConfiguration(false, false)

    lazy val ci_di_wi: IncrementalConfiguration = IncrementalConfiguration(true, true, true, false)
    lazy val ci_wi_cy: IncrementalConfiguration = IncrementalConfiguration(true, false)
    lazy val di_wi_cy: IncrementalConfiguration = IncrementalConfiguration(false)

    lazy val allOptimisations: IncrementalConfiguration = IncrementalConfiguration()

    /** A list of all possible configurations for the incremental analysis (CY requires WI). */
    lazy val allConfigurations: List[IncrementalConfiguration] =
      List(
        // No optimisations
        noOptimisations,
        // One optimisation
        ci,
        di,
        wi,
        // Two optimisations
        ci_di,
        ci_wi,
        di_wi,
        //wi_cy,
        // Three optimisations
        ci_di_wi,
        //ci_wi_cy,
        //di_wi_cy,
        // Four optimisations
        //allOptimisations,
      )

    case class InvalidConfigurationException(message: String, config: IncrementalConfiguration) extends Exception(message)

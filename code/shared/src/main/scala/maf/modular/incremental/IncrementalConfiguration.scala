package maf.modular.incremental

/** Select which optimisations of the incremental analysis should be enabled. */
trait IncrementalConfiguration {

  /**
   * Indicates whether components should be invalidated when they are no longer spawned.
   * This involves a possibly recursive traversal of spawn and cycle detection.
   * This includes the deletion of the corresponding return addresses.
   */
  val componentInvalidation: Boolean

  /** Indicates whether (read) dependencies should be removed when they are no longer inferred. */
  val dependencyInvalidation: Boolean

  /**
   * Indicates whether writes should be refined using provenance tracking.
   * This includes the removal of addresses that are no longer written.
   */
  val writeInvalidation: Boolean

  /**
   * Indicates whether writeInvalidation should be improved upon by tracking which values depend on each other,
   * and by invalidating (lowering) values at addresses in a SCA.
   */
  val cyclicValueInvalidation: Boolean

  if (cyclicValueInvalidation && !writeInvalidation)
    throw new Exception("Illegal configuration state: cyclic value invalidation requires write invalidation.")

  private def booleanToString(b: Boolean): String = if (b) "enabled" else "disabled"

  override def toString: String =
    s"""****** Incremental configuration ******
       | + Component invalidation: ${booleanToString(componentInvalidation)}
       | + Dependency invalidation: ${booleanToString(dependencyInvalidation)}
       | + Write invalidation: ${booleanToString(writeInvalidation)}
       |    - Cycle detection: ${booleanToString(cyclicValueInvalidation)}
       |***************************************""".stripMargin

  def shortName(): String = {
    val ci = if (componentInvalidation) "CI" else "xx"
    val di = if (dependencyInvalidation) "DI" else "xx"
    val wi = if (writeInvalidation) {
      if (cyclicValueInvalidation) "WI+CY" else "WI"
    } else "xx"
    s"$ci-$di-$wi"
  }
}

/** Provides configurations for the incremental analysis. */
object IncrementalConfiguration {

  case class Config(
      componentInvalidation: Boolean = true,
      dependencyInvalidation: Boolean = true,
      writeInvalidation: Boolean = true,
      cyclicValueInvalidation: Boolean = true)
      extends IncrementalConfiguration

  // For simplicity, already instantiate all possible configurations here so they can be referred by name.

  lazy val noOptimisations: Config = Config(false, false, false, false)

  lazy val ci: Config = Config(true, false, false, false)
  lazy val di: Config = Config(false, true, false, false)
  lazy val wi: Config = Config(false, false, true, false)
  lazy val wi_cy: Config = Config(false, false)

  lazy val ci_di: Config = Config(true, true, false, false)
  lazy val ci_wi: Config = Config(true, false, true, false)
  lazy val di_wi: Config = Config(false, true, true, false)

  lazy val ci_di_wi: Config = Config(true, true, true, false)

  lazy val allOptimisations: Config = Config() // ci_di_wi_cy

  lazy val allConfigurations: List[Config] = List(noOptimisations, ci, di, wi, wi_cy, ci_di, ci_wi, di_wi, ci_di_wi, allOptimisations)
}

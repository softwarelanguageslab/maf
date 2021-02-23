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
}

object NoOptimisations extends IncrementalConfiguration {
  val componentInvalidation: Boolean = false
  val dependencyInvalidation: Boolean = false
  val writeInvalidation: Boolean = false
  val cyclicValueInvalidation: Boolean = false
}

object AllOptimisations extends IncrementalConfiguration {
  val componentInvalidation: Boolean = true
  val dependencyInvalidation: Boolean = true
  val writeInvalidation: Boolean = true
  val cyclicValueInvalidation: Boolean = true
}

case class CustomOptimisations(
    componentInvalidation: Boolean = true,
    dependencyInvalidation: Boolean = true,
    writeInvalidation: Boolean = true,
    cyclicValueInvalidation: Boolean = true)
    extends IncrementalConfiguration

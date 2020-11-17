package maf.util

object Annotations {
  /** Marks a procedure or method that uses an unsound assumption. */
  class unsound(reason: String = "")      extends scala.annotation.StaticAnnotation
  /** Indicates a doubt about the soundness of a given piece of code. */
  class maybeUnsound(reason: String = "") extends unsound

  class toCheck(reason: String = "")      extends scala.annotation.StaticAnnotation

  /** Marks data structures that are destructively updated. */
  class mutable                           extends scala.annotation.StaticAnnotation

  class assume(assumption: String = "")   extends scala.annotation.StaticAnnotation

  /** Added to methods/functions that may directly update the data structures of an analysis in a non-monotonic way. */
  class nonMonotonicUpdate                extends scala.annotation.StaticAnnotation
}

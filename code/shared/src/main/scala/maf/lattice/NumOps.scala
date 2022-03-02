package maf.lattice

/** Utilities for working with BigInts. */
object NumOps:

    /** Used to convert a BigInt to a Double, but warns if the value is too big/small. */
    // TODO Perhaps use BigDecimals as well.
    implicit def bigIntToDouble(i: BigInt): Double =
      if i.isValidDouble then i.doubleValue else throw new Exception(s"Illegal conversion of $i to Double.")

    /** Used to convert a BigInt to an Int, but warns if the value is too big/small. */
    def bigIntToInt(i: BigInt): Int = if i.isValidInt then i.intValue else throw new Exception(s"Illegal conversion of $i to Int.")

    /** Used to create a BigInt from a String. Returns an Option indicating whether the conversion was successful. */
    def bigIntFromString(s: String): Option[BigInt] =
      try Some(BigInt(s, 10)) // May throw a NumberFormatException.
      catch case _: Throwable => None

package maf.lattice

import scala.util.Random

/** Various implementations of mathematical utilities functions. */
object MathOps:
    def ceil(a: Double): Double = scala.math.ceil(a)

    /** Modulo in Scheme and Scala are different. This implements the same behavior as Scheme's modulo */
    def modulo(n1: BigInt, n2: BigInt): BigInt =
      if n1.signum * n2.signum < 0 then
          /* different sign, behaviour not the same between Scheme and Scala, adjust it */
          (n2.abs - n1.abs % n2.abs) % n2.abs * (if n2 < 0 then -1 else 1)
      else
          /* same sign, same behaviour */
          n1 % n2

    /** Remainder in Scheme has the same behavior of Scala's modulo. */
    def remainder(n1: BigInt, n2: BigInt): BigInt = n1 % n2

    def random(n: BigInt): BigInt = (BigInt(n.bitLength, new Random()) % n).abs

    def random(n: Double): Double = scala.math.abs(scala.util.Random.nextDouble() % n)

    /** Round in Scheme and Scala are different. This implements the same behaviour as Scheme's round. */
    def round(n: Double): Double =
        val frac = n % 1 /* Fractional part of n */
        /* In the case of a fraction part equaling 0.5, rounding is done towards the even number. */
        if (scala.math.abs(frac) == 0.5) && (((n > 0) && ((scala.math.abs(n - frac) % 2) == 0)) || ((n < 0) && (((n - frac) % 2) == -1))) then
            scala.math.round(n).toDouble - 1
        else scala.math.round(n).toDouble

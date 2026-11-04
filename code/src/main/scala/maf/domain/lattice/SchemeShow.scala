package maf.domain.lattice

import maf.util.Show

/** Shows base values the way they are written in Scheme. */
object SchemeShow:
    implicit val stringShow: Show[String] = new Show[String] {
        def show(s: String): String = s""""$s""""
    }
    implicit val boolShow: Show[Boolean] = new Show[Boolean] {
        def show(b: Boolean): String =
            if b then "#t"
            else "#f"
    }
    implicit val intShow: Show[BigInt] = new Show[BigInt] {
        def show(i: BigInt): String = s"$i"
    }
    implicit val doubleShow: Show[Double] = new Show[Double] {
        def show(d: Double): String = s"$d"
    }
    implicit val charShow: Show[Char] = new Show[Char] {
        def show(c: Char): String = s"#\\$c"
    }
    // Not implicit because it would conflict with stringShow.
    val symShow: Show[String] = new Show[String] {
        def show(s: String): String = s"'$s"
    }

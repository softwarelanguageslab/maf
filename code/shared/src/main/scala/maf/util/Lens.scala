package maf.util

import scala.deriving.Mirror

trait Lens[S]:
    def modify[I](trans: (((S, I) => S), (S) => I))(f: I => I): S => S = (s: S) =>
        val inner = trans._2(s)
        val st = s
        trans._1(st, f(inner))

    def modifyHasChanged[I](trans: (((S, I) => S), (S) => I))(f: I => I): S => (Boolean, S) = (s: S) =>
        val inner = trans._2(s)
        val st = s
        val newSt = trans._1(st, f(inner))
        (newSt != st, s)

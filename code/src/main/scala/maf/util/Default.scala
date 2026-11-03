package maf.util

import maf.core.*

trait Default[A]:
    def default: A

object Default:
    given latticeBottomDefault[L: Lattice]: Default[L] with
        def default: L = Lattice[L].bottom

    given setDefaultIsEmpty[T]: Default[Set[T]] with
        def default: Set[T] = Set()

    given unitDefault[T]: Default[Unit] with
        def default: Unit = ()

    def errorIfDefault[A]: Default[A] = new Default[A]:
        def default: A = throw new Exception("no default")

    def default[A: Default]: A = summon[Default[A]].default

package maf.util

trait Default[A]:
    def default: A

object Default:
    given setDefaultIsEmpty[T]: Default[Set[T]] with
        def default: Set[T] = Set()

    given unitDefault[T]: Default[Unit] with
        def default: Unit = ()

    def errorIfDefault[A]: Default[A] = new Default[A]:
        def default: A = throw new Exception("no default")

    def default[A: Default]: A = summon[Default[A]].default

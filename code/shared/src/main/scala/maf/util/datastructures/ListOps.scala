package maf.util.datastructures

object ListOps:

    // Source: https://stackoverflow.com/a/14740340
    implicit class Crossable[X](xs: Iterable[X]):
        def cartesian[Y](ys: Iterable[Y]): Iterable[(X, Y)] = for
            x <- xs
            y <- ys
        yield (x, y)

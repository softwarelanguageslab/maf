package maf.util.datastructures

object ListOps:

    // Source: https://stackoverflow.com/a/14740340
    implicit class Crossable[X](xs: Iterable[X]):
        def cartesian[Y](ys: Iterable[Y]): Iterable[(X, Y)] = for
            x <- xs
            y <- ys
        yield (x, y)

    implicit class Filterable[X](xs: List[X]):
        def filterDuplicates: List[X] =
            var seen: Set[X] = Set()
            var result: List[X] = List()
            for x <- xs do
                if !seen.contains(x) then
                    result = x :: result
                    seen = seen + x

            result

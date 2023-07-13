package maf.util

import maf.core.Address

object StoreUtil:
    private def crop(string: String, length: Option[Int]): String = if string.length <= length.getOrElse(Int.MaxValue) then string else string.take(length.getOrElse(Int.MaxValue)) + "..."
    def storeString[V](store: Map[Address, V], primitives: Boolean = false, crp: Option[Int] = None): String =
        val strings = store.map({ case (a, v) => s"${StringUtil.toWidth(crop(a.toString, crp), 50)}: $v" })
        val contents = (if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))).toList.sorted
        val size = contents.size
        val longest = contents.map(_.length).foldLeft(0)(Math.max)
        val infoString = "σ" * longest + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
        contents.mkString(infoString, "\n", "\n" + "σ" * longest)


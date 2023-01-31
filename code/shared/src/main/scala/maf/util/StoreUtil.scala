package maf.util

import maf.core.Address

object StoreUtil:
    private def crop(string: String, length: Option[Int]): String = if string.length <= length.getOrElse(Int.MaxValue) then string else string.take(length.getOrElse(Int.MaxValue)) + "..."
    def storeString[V](store: Map[Address, V], primitives: Boolean = false, crp: Option[Int] = None): String =
        val strings = store.map({ case (a, v) => s"${StringUtil.toWidth(crop(a.toString, crp), 50)}: $v" })
        val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
        val size = filtered.size
        val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
        filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)

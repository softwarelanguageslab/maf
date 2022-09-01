package maf.util

import maf.core.Address

object StoreUtil:
    def storeString[V](store: Map[Address, V], primitives: Boolean = false): String =
        val strings = store.map({ case (a, v) => s"${StringUtil.toWidth(a.toString, 50)}: $v" })
        val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
        val size = filtered.size
        val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
        filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)

package maf.util.ace

import scala.scalajs.js

trait AceEditor extends js.Object:
    def getValue(): String
    def setValue(vlu: String): Unit

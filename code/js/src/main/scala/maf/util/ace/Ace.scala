package maf.util.ace

import scala.scalajs.js.annotation.*
import scala.scalajs.js
import org.scalajs.dom.raw.HTMLElement

trait Ace extends js.Object:
    def edit(element: HTMLElement, options: js.Any): AceEditor

object Ace:
    @JSGlobal("ace")
    @js.native
    def ace: Ace = js.native

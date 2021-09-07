package maf.web.utils

// Scala.js-related imports
import org.scalajs.dom
import org.scalajs.dom._

object Button:
    def apply(label: String)(handler: => Unit): html.Button =
        val button = document.createElement("button").asInstanceOf[html.Button]
        button.onclick = (_: dom.raw.MouseEvent) => handler
        button.innerHTML = label //TODO: escape certain chars?
        return button

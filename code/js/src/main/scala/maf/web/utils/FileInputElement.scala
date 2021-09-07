package maf.web.utils

// Scala.js-related imports
import org.scalajs.dom
import org.scalajs.dom._

object FileInputElement:
    def apply(handler: String => Unit): html.Input =
        val input = document.createElement("input").asInstanceOf[html.Input]
        input.setAttribute("type", "file")
        input.addEventListener(
          "change",
          (evtUpload: dom.Event) => {
            val file = input.files.item(0)
            val reader = new dom.FileReader()
            reader.onload = (evtLoad: dom.Event) => handler(reader.result.asInstanceOf[String])
            reader.readAsText(file)
          },
          false
        )
        return input

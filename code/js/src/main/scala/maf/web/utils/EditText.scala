package maf.web.utils

import org.scalajs.dom
import org.scalajs.dom._

object EditText:
    def apply(handler: String => Unit): html.Div =
        // Create the container for the edit text and the button
        val container = document.createElement("div").asInstanceOf[html.Div]

        // Create the input
        val input = document.createElement("textarea").asInstanceOf[html.TextArea]
        input.cols = 40
        input.rows = 10

        container.appendChild(input)

        // Create the submission
        val submit = document.createElement("input").asInstanceOf[html.Input]
        submit.`type` = "submit"
        submit.value = "Start analysis"

        submit.addEventListener("click",
                                (evt: dom.Event) =>
                                    evt.preventDefault()
                                    handler(input.value)
        )

        val brk = document.createElement("br")

        container.appendChild(brk)
        container.appendChild(submit)
        container

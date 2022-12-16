package maf.web.utils

import org.scalajs.dom
import scala.scalajs.js
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement

class EditText(handler: String => Unit):
    private var container: html.Div = _
    private var submit: html.Element = _

    import maf.util.ace.Ace.*
    def init(): EditText =

        // Create the container for the edit text and the button
        val container = document.createElement("div").asInstanceOf[html.Div]
        // Create a space for the editor
        val editorContainer = document.createElement("div").asInstanceOf[html.Div]
        editorContainer.classList.add("editor")
        container.appendChild(editorContainer)

        // Create the input
        val editor = ace.edit(editorContainer, js.Dynamic.literal(mode = "ace/mode/scheme", selectionStyle = "text"))

        // Create the submission
        submit = document.createElement("button").asInstanceOf[html.Input]
        submit.classList.add("btn")
        submit.innerText = "Start analysis"

        submit.addEventListener("click",
                                (evt: dom.Event) =>
                                    evt.preventDefault()
                                    submit.classList.add("disabled")
                                    submit.innerText = "Analysis is running..."
                                    handler(editor.getValue())
        )

        val brk = document.createElement("br")

        container.appendChild(brk)
        container.appendChild(submit)
        this.container = container
        this

    def render(): html.Element = this.container

    def reset(): Unit =
        submit.innerText = "Start analysis"
        submit.classList.remove("disabled")

    def appendChild(element: HTMLElement): Unit =
        this.container.appendChild(element)

object EditText:
    def apply(handler: String => Unit): EditText =
        new EditText(handler).init()

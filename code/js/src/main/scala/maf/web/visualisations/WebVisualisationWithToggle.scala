package maf.web.visualisations

import maf.web.utils._

import org.scalajs.dom

trait WebVisualisationWithToggle extends WebVisualisation:

    // can be enabled and/or disabled ...
    private var enabled = true
    // ... using a button
    private lazy val toggleButton: dom.html.Button = Button("Hide graph visualisation") {
      if enabled then
          enabled = false
          toggleButton.innerHTML = "Show graph visualisation"
          svgDiv.removeChild(svgNode)
      else
          enabled = true
          toggleButton.innerHTML = "Hide graph visualisation"
          svgDiv.appendChild(svgNode)
          refresh() // do a full refresh
    }

    // when disabled, updates do not happen before/after stepping
    override def beforeStep() =
      if enabled then super.beforeStep()

    override def afterStep() =
      if enabled then super.afterStep()

    // add the toggle button before the webvisualisation
    node.insertBefore(toggleButton, node.firstChild)

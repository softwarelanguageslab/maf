package maf.web.visualisations

import maf.web.utils._

import org.scalajs.dom

trait WebVisualisationWithToggle extends WebVisualisation {

  // can be enabled and/or disabled ...
  private var enabled = true
  // ... using a button
  private lazy val toggleButton = Button("Toggle visualisation") {
    if (enabled) {
      enabled = false
      svgDiv.removeChild(svgNode)
    } else {
      enabled = true
      svgDiv.appendChild(svgNode)
      refresh() // do a full refresh
    }
  }

  // when disabled, updates do not happen before/after stepping
  override def beforeStep() = {
    if(enabled) {
        super.beforeStep()
    }
  }
  
  override def afterStep() = {
      if(enabled) {
          super.afterStep()
      }
  }

  // add the toggle button before the webvisualisation
  node.insertBefore(toggleButton, node.firstChild)
}

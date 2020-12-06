package maf.web

import maf.core.Expression
import maf.modular.DependencyTracking
import maf.modular.incremental.IncrementalModAnalysis
import maf.web.WebVisualisation._
import maf.web.WebVisualisationIncremental._
import scala.scalajs.js

object WebVisualisationIncremental {
  val __CSS_DELETED_NODE__ = "node_deleted"
  val __CSS_DELETED_EDGE__ = "edge_deleted"
  val __SVG_DELETED_ARROW__ = "endarrow_deleted"
}

class WebVisualisationIncremental[Expr <: Expression](override val analysis: IncrementalModAnalysis[Expr] with DependencyTracking[Expr])
  extends WebVisualisation(analysis) {

  def deletedComponent(cmp: analysis.Component): Boolean = !analysis.visited.contains(cmp) && !analysis.workList.contains(cmp)

  override def classifyNodes(): Unit = {
    super.classifyNodes()
    nodes.classed(__CSS_DELETED_NODE__, (node: Node) => deletedComponent(node.component))
  }

  override def classifyEdges(): Unit = {
    super.classifyEdges()
    edges.classed(__CSS_DELETED_EDGE__, (edge: Edge) => deletedComponent(edge.target.component))
  }

  override def setupMarker(svg: JsAny): js.Dynamic = {
    super.setupMarker(svg)
    val marker = svg.select("defs").append("markerInc")
                                   .attr("id",__SVG_DELETED_ARROW__)
                                   .attr("viewBox","-0 -5 10 10")
                                   .attr("refX",0)
                                   .attr("refY",0)
                                   .attr("orient","auto")
                                   .attr("markerWidth",5)
                                   .attr("markerHeight",5)
                                   .attr("fill","darkgray")
                                   .attr("stroke","darkgray")
    marker.append("svg:path").attr("d", "M 0,-5 L 10 ,0 L 0,5")
  }

}

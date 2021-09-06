package maf.web.visualisations.incremental

import maf.core.Expression
import maf.modular.incremental.{IncrementalGlobalStore, IncrementalModAnalysis}
import maf.web.visualisations._
import maf.web.visualisations.incremental.WebVisualisationIncremental._

import maf.web.utils.JSHelpers._

import scala.scalajs.js

trait VisualisableIncrementalModAnalysis[Expr <: Expression]
    extends IncrementalModAnalysis[Expr]
    with IncrementalGlobalStore[Expr]
    with WebVisualisationAnalysis[Expr]
    with AddressVisualisationAnalysis[Expr]:
    var recursive: Set[Component] = Set() // Collects the set of recursive components, since self-edges are omitted in the set `cachedSpawns`.

    override def intraAnalysis(cmp: Component): VisualisableIntraAnalysis

    trait VisualisableIntraAnalysis extends IncrementalIntraAnalysis with DependencyTrackingIntra with GlobalStoreIntra:
        override def refineComponents(): Unit =
            if C.contains(component) then recursive = recursive + component
            else recursive = recursive - component
            super.refineComponents()

    /** Hook that is called after a file is loaded. */
    def initAnalysis(): Unit

object WebVisualisationIncremental:
    val __CSS_DELETED_NODE__ = "node_deleted"
    val __CSS_DELETED_EDGE__ = "edge_deleted"
    val __SVG_DELETED_ARROW__ = "endarrow_deleted"
    val __CSS_MAIN_NODE__ = "node_main"

class WebVisualisationIncremental(
    override val analysis: VisualisableIncrementalModAnalysis[_],
    width: Int,
    height: Int)
    extends WebVisualisation(width, height):

    def deletedComponent(cmp: analysis.Component): Boolean = !analysis.visited.contains(cmp) && !analysis.workList.contains(cmp)

    override def classifyNodes(): Unit =
        super.classifyNodes()
        nodes.classed(__CSS_DELETED_NODE__,
                      (node: Node) =>
                        node match {
                          case node: CmpNode => deletedComponent(node.component);
                          case _             => false
                        }
        )
        nodes.classed(__CSS_MAIN_NODE__,
                      (node: Node) =>
                        node match {
                          case node: CmpNode => node.component == analysis.initialComponent;
                          case _             => false
                        }
        )

    override def classifyEdges(): Unit =
        super.classifyEdges()
        edges.classed(
          __CSS_DELETED_EDGE__,
          (edge: Edge) =>
            (edge.source, edge.target) match {
              case (source, target) if source.isInstanceOf[CmpNode] && target.isInstanceOf[CmpNode] =>
                val src = source.asInstanceOf[CmpNode].component
                val trg = target.asInstanceOf[CmpNode].component
                deletedComponent(src) || deletedComponent(trg) || (
                  !analysis
                    .cachedSpawns(src)
                    .contains(
                      trg
                    )
                ) && !(src == trg && analysis.recursive.contains(src))
              case _ =>
            }
        ) // Check that it is not a currently existing self-edge.

    /**
     * Ensures all new nodes and edges in the analysis are drawn. May not shown nodes that have ceased to exist.
     *
     * @note
     *   No nodes and edges are deleted. Those that are deleted by the analysis will still be drawn, but will be visualised differently.
     * @note
     *   Nodes and edges that were created and deleted by the analysis since the last update of the visualisation data will not be shown using this
     *   method.
     */
    override def refreshData(): Unit =
        // Add all components currently in the analysis.
        analysis.visited.foreach { cmp =>
            val node = getNode(cmp)
            nodesData += node
        }
        // Add all edges currently in the analysis.
        nodesData.foreach {
          case sourceNode: CmpNode =>
            val targets = analysis.dependencies(sourceNode.component)
            targets.foreach { target =>
                val targetNode = getNode(target)
                val edge = getEdge(sourceNode, targetNode)
                edgesData += edge
            }
          case _ =>
        }

    override def refreshDataAfterStep() =
        val sourceNode = getNode(prevComponent)
        // Add new edges.
        analysis.dependencies(prevComponent).foreach { otherCmp =>
            val targetNode = getNode(otherCmp)
            val edge = getEdge(sourceNode, targetNode)
            nodesData += targetNode
            edgesData += edge
        }

    override def setupMarker(svg: JsAny): js.Dynamic =
        super.setupMarker(svg)
        val marker = svg
          .select("defs")
          .append("marker")
          .attr("id", __SVG_DELETED_ARROW__)
          .attr("viewBox", "-0 -5 10 10")
          .attr("refX", 0)
          .attr("refY", 0)
          .attr("orient", "auto")
          .attr("markerWidth", 5)
          .attr("markerHeight", 5)
          .attr("fill", "darkgray")
          .attr("stroke", "darkgray")
        marker.append("svg:path").attr("d", "M 0,-5 L 10 ,0 L 0,5")

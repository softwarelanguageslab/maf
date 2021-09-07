package maf.web.visualisations.incremental

import maf.modular._
import maf.web.visualisations._
import maf.web.visualisations.incremental.AddressVisualisationIncremental._

import maf.web.utils.JSHelpers._

import scala.scalajs.js

object AddressVisualisationIncremental:
    val __CSS_DELADDR_NODE__ = "del_addr_node" // Deleted address (node).
    val __CSS_WRITE_EDGE__ = "write_edge"
    val __CSS_DELREAD_EDGE__ = "del_read" // Deleted read.
    val __CSS_DELWRITE_EDGE__ = "del_write"
    val __CSS_DELETED_EDGE__ = "del_addr_edge"
    val __CSS_EXISTING_WRITE__ = "existingwrite"
    val __CSS_FORMER_WRITE__ = "formerwrite"
    val __SVG_WRITE_ARROW__ = "writearrow"
    val __SVG_DREAD_ARROW__ = "delreadarrow"
    val __SVG_DWRITE_ARROW__ = "delwritearrow"

trait AddressVisualisationIncremental extends WebVisualisationIncremental with AddressVisualisation:

    var oldWriteDeps: Map[analysis.Component, Set[analysis.Addr]] = Map().withDefaultValue(Set.empty)

    def isWriteEdge(edge: Edge): Boolean = edge.source.isInstanceOf[CmpNode] && edge.target.isInstanceOf[AddrNode]

    def isExistingReadEdge(edge: Edge): Boolean =
      analysis.cachedReadDeps(edge.target.asInstanceOf[CmpNode].component).contains(AddrDependency(edge.source.asInstanceOf[AddrNode].address))

    def isExistingWriteEdge(edge: Edge): Boolean =
      analysis.cachedWrites(edge.source.asInstanceOf[CmpNode].component).contains(edge.target.asInstanceOf[AddrNode].address)

    // Now also add edges for writes.
    override def refreshData(): Unit =
        super.refreshData()
        analysis.provenance.foreach { case (addr, prov) =>
          val addrNode = getNode(addr)
          nodesData += addrNode // Should normally already be there.
          prov.foreach { case (cmp, _) =>
            val writerNode = getNode(cmp)
            val edge = getEdge(writerNode, addrNode) // Edge from writer -> addr.
            edgesData += edge
          }
        }

    override def refreshDataAfterStep(): Unit =
        super.refreshDataAfterStep()
        val writerNode = getNode(prevComponent)
        analysis.cachedWrites(prevComponent).foreach { addr =>
            val addrNode: AddrNode = getNode(addr)
            val edge = getEdge(writerNode, addrNode)
            nodesData += addrNode
            edgesData += edge
        }
        oldWriteDeps = analysis.cachedWrites

    override def classifyNodes(): Unit =
        super.classifyNodes()
        nodes.classed(__CSS_DELADDR_NODE__,
                      (node: Node) =>
                        node match {
                          case n: AddrNode => !analysis.store.contains(n.address)
                          case _           => false
                        }
        )

    override def classifyEdges(): Unit =
        super.classifyEdges()
        edges
          .classed(__CSS_DELREAD_EDGE__, (edge: Edge) => isReadEdge(edge) && !isExistingReadEdge(edge))
          .classed(__CSS_WRITE_EDGE__, (edge: Edge) => isWriteEdge(edge) && isExistingWriteEdge(edge))
          .classed(__CSS_DELWRITE_EDGE__, (edge: Edge) => isWriteEdge(edge) && !isExistingWriteEdge(edge))

    override def classifyLabels(): Unit =
        super.classifyLabels()
        labels
          .classed(__CSS_EXISTING_WRITE__, (edge: Edge) => isWriteEdge(edge) && isExistingWriteEdge(edge))
          .classed(__CSS_FORMER_WRITE__, (edge: Edge) => isWriteEdge(edge) && !isExistingWriteEdge(edge))

    override def setupMarker(svg: JsAny): js.Dynamic =
        super.setupMarker(svg)
        newMarker(svg, __SVG_WRITE_ARROW__).attr("fill", "tan").attr("stroke", "tan")
        newMarker(svg, __SVG_DREAD_ARROW__).attr("fill", "lightgray").attr("stroke", "lightgray")
        newMarker(svg, __SVG_DWRITE_ARROW__).attr("fill", "lightgray").attr("stroke", "lightgray")

    // Based on http://bl.ocks.org/jhb/5955887.
    // TODO: get text in the right place, in the right colour and remove old text.
    override def refreshHook(): Unit =
        super.refreshHook()
        // Update the labels on the edges
        val labelsUpdate = labels.data(edgesData, (e: Edge) => (e.source.data(), e.target.data()))
        labels = labelsUpdate
          .enter()
          .remove("text")
          .append("text")
          .style("pointer-events", "none")
          .attr("class", "labels")
        labelsUpdate
          .append("textPath")
          .attr("xlink:href", (e: Edge) => "#" + e.id)
          .text((e: Edge) =>
            e.target match {
              case node: AddrNode => analysis.provenance(node.address)(e.source.asInstanceOf[CmpNode].component).toString.take(15)
              case _              => ""
            }
          )
        classifyLabels()
        labelsUpdate.exit().remove()

// Ensures all edges and addresses remain shown.
trait RetainAllIncremental extends RetainAll:
    this: AddressVisualisationIncremental =>
    override def deleteOnStep(cmp: analysis.Component): Unit =
        super.deleteOnStep(cmp)
        val writerNode = getNode(cmp)
        oldWriteDeps(cmp).foreach { addr =>
            val addrNode: AddrNode = getNode(addr)
            val edge = getEdge(writerNode, addrNode)
            edgesData -= edge
        }

// Only shows the addresses read and written by the component last analysed.
trait RetainUpdatedIncremental extends RetainUpdated:
    this: AddressVisualisationIncremental =>
    override def deleteOnStep(cmp: analysis.Component): Unit =
        super.deleteOnStep(cmp)
        edgesData = edgesData.filterNot(_.target.isInstanceOf[AddrNode]) // For Incremental Address Visualisation.

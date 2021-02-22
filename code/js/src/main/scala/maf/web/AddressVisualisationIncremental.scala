package maf.web

import maf.modular._
import maf.web.AddressVisualisationIncremental._
import maf.web.WebVisualisation._

import scala.scalajs.js

object AddressVisualisationIncremental {
  val __CSS_DELADDR_NODE__ = "del_addr_node" // Deleted address (node).
  val __CSS_WRITE_EDGE__ = "write_edge"
  val __CSS_DELREAD_EDGE__ = "del_read" // Deleted read.
  val __CSS_DELWRITE_EDGE__ = "del_write"
  val __CSS_DELETED_EDGE__ = "del_addr_edge"
  val __SVG_WRITE_ARROW__ = "writearrow"
  val __SVG_DREAD_ARROW__ = "delreadarrow"
  val __SVG_DWRITE_ARROW__ = "delwritearrow"
}

trait AddressVisualisationIncremental extends WebVisualisationIncremental with AddressVisualisation {

  var oldWriteDeps: Map[analysis.Component, Set[analysis.Addr]] = Map().withDefaultValue(Set.empty)

  // Now also add edges for writes.
  override def refreshData(): Unit = {
    super.refreshData()
    analysis.provenance.foreach { case (addr, prov) =>
      val addrNode = getNode(addr)
      nodesData += addrNode // Should normally already be there.
      prov.foreach { case (cmp, value) =>
        val writerNode = getNode(cmp)
        val edge = getEdge(writerNode, addrNode) // Edge from writer -> addr.
        edgesData += edge
      }
    }
  }

  override def refreshDataAfterStep(cmp: analysis.Component, oldCmpDeps: Set[analysis.Component]): Unit = {
    super.refreshDataAfterStep(cmp, oldCmpDeps)
    val writerNode = getNode(cmp)
    analysis.cachedWrites(cmp).foreach { addr =>
      val addrNode: AddrNode = getNode(addr)
      val edge = getEdge(writerNode, addrNode)
      nodesData += addrNode
      edgesData += edge
    }
    oldWriteDeps = analysis.cachedWrites
  }

  override def classifyNodes(): Unit = {
    super.classifyNodes()
    nodes.classed(__CSS_DELADDR_NODE__,
                  (node: Node) =>
                    node match {
                      case n: AddrNode => !analysis.store.contains(n.address)
                      case _           => false
                    }
    )
  }

  override def classifyEdges(): Unit = {
    super.classifyEdges()
    edges
      .classed(
        __CSS_DELREAD_EDGE__,
        (edge: Edge) =>
          edge.source.isInstanceOf[AddrNode] && edge.target.isInstanceOf[CmpNode] &&
            !analysis.cachedReadDeps(edge.target.asInstanceOf[CmpNode].component).contains(AddrDependency(edge.source.asInstanceOf[AddrNode].address))
      )
      .classed(
        __CSS_WRITE_EDGE__,
        (edge: Edge) =>
          edge.source.isInstanceOf[CmpNode] && edge.target.isInstanceOf[AddrNode] &&
            analysis.cachedWrites(edge.source.asInstanceOf[CmpNode].component).contains(edge.target.asInstanceOf[AddrNode].address) //&&
        // analysis.provenance(edge.target.asInstanceOf[AddrNode].address)(edge.source.asInstanceOf[CmpNode].component) != analysis.lattice.bottom
      )
      .classed(
        __CSS_DELWRITE_EDGE__,
        (edge: Edge) =>
          edge.source.isInstanceOf[CmpNode] && edge.target.isInstanceOf[AddrNode] &&
            !analysis.cachedWrites(edge.source.asInstanceOf[CmpNode].component).contains(edge.target.asInstanceOf[AddrNode].address) //&&
        // analysis.provenance(edge.target.asInstanceOf[AddrNode].address)(edge.source.asInstanceOf[CmpNode].component) == analysis.lattice.bottom
      )
  }

  override def setupMarker(svg: JsAny): js.Dynamic = {
    super.setupMarker(svg)
    newMarker(svg, __SVG_WRITE_ARROW__).attr("fill", "tan").attr("stroke", "tan")
    newMarker(svg, __SVG_DREAD_ARROW__).attr("fill", "lightgray").attr("stroke", "lightgray")
    newMarker(svg, __SVG_DWRITE_ARROW__).attr("fill", "lightgray").attr("stroke", "lightgray")

  }

  /*
  // Based on http://bl.ocks.org/jhb/5955887.
  override def refreshHook(): Unit = {
    super.refreshHook()
    // Update the labels on the edges
    val labelsUpdate = labels.data(edgesData, (e: Edge) => (e.source.data(), e.target.data()))
    labels = labelsUpdate
      .enter()
      .append("text")
      .style("pointer-events", "none")
      .attr("class", "labels")
      .attr("fill", "black")
    labelsUpdate
      .append("textPath")
      .attr("xlink:href", (e: Edge) => "#" + e.id)
      .text(
        "Hello world"
        //(e: Edge) =>
        //e.target match {
        //  case node: AddrNode => analysis.provenance(node.address)(e.source.asInstanceOf[CmpNode].component).toString
        //  case _              => "NONE"
        // }
      )
    labelsUpdate.exit().remove()
  }

  override def onTickHook(): Unit =
    super.onTickHook()
  //TODO
   */
}

// Ensures all edges and addresses remain shown.
trait RetainAllIncremental extends RetainAll {
  this: AddressVisualisationIncremental =>
  override def deleteOnStep(cmp: analysis.Component): Unit = {
    super.deleteOnStep(cmp)
    val writerNode = getNode(cmp)
    oldWriteDeps(cmp).foreach { addr =>
      val addrNode: AddrNode = getNode(addr)
      val edge = getEdge(writerNode, addrNode)
      edgesData -= edge
    }
  }
}

// Only shows the addresses read and written by the component last analysed.
trait RetainUpdatedIncremental extends RetainUpdated {
  this: AddressVisualisationIncremental =>
  override def deleteOnStep(cmp: analysis.Component): Unit = {
    super.deleteOnStep(cmp)
    edgesData = edgesData.filterNot(_.target.isInstanceOf[AddrNode]) // For Incremental Address Visualisation.
  }
}

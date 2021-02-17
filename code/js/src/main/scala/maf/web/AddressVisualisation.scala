package maf.web

import maf.modular._
import maf.modular.scheme.PrmAddr
import maf.web.AddressVisualisation.{__CSS_ADDRESS_NODE__, __CSS_READ_EDGE__}

object AddressVisualisation {
  val __CSS_ADDRESS_NODE__ = "address_node"
  val __CSS_READ_EDGE__ = "read_edge"
  //val __CSS_DELADDR_NODE__ = "deladdr_node" // TODO for incremental variant
}

/** Adds the visualisation of addresses to the web visualisation. */
trait AddressVisualisation extends WebVisualisation {

  var adrNodesColl: Map[analysis.Addr, AddrNode] = Map()
  var oldDeps: Map[Dependency, Set[analysis.Component]] = Map()

  class AddrNode(val address: analysis.Addr) extends Node {
    def displayText(): String = s"${address.toString} [${analysis.store.getOrElse(address, analysis.lattice.bottom).toString.take(10)}]"

    def data(): Any = address
  }

  def getNode(addr: analysis.Addr): AddrNode = adrNodesColl.get(addr) match {
    case None =>
      val newNode = new AddrNode(addr)
      adrNodesColl += (addr -> newNode)
      newNode
    case Some(existingNode) => existingNode
  }

  override def refreshData(): Unit = {
    super.refreshData()
    // Also add nodes for the addresses.
    analysis.store.keySet.foreach { addr =>
      if (!addr.isInstanceOf[PrmAddr]) {
        val node = getNode(addr)
        nodesData += node
      }
    }
    nodesData.foreach {
      case addrNode: AddrNode =>
        // Using the standard Global Store, we can only visualise read dependencies.
        val readers: Set[analysis.Component] = analysis.deps(AddrDependency(addrNode.address))
        readers.foreach { reader =>
          val readerNode = getNode(reader)
          val edge = getEdge(addrNode, readerNode) // Edges from addr -> reader.
          edgesData += edge
        }
      case _ => // Other cases may be handled in super-/subclasses
    }
  }

  override def refreshDataAfterStep(cmp: analysis.Component, oldCmpDeps: Set[analysis.Component]): Unit = {
    val readerNode = getNode(cmp)
    // Remove old edges.
    oldDeps.filter(_._2.contains(cmp)).keySet.foreach {
      case AddrDependency(addr) =>
        val addrNode: AddrNode = getNode(addr)
        val edge = getEdge(addrNode, readerNode)
        edgesData -= edge
      case _ =>
    }
    super.refreshDataAfterStep(cmp, oldCmpDeps) // Do this after deleting the nodes, as this adds the new nodes.
    oldDeps = analysis.deps // Avoid having to override stepAnalysis etc. by saving the new state of the dependencies.
  }

  override def classifyNodes(): Unit = {
    super.classifyNodes()
    nodes.classed(__CSS_ADDRESS_NODE__,
                  (node: Node) =>
                    node match {
                      case _: AddrNode => true;
                      case _           => false
                    }
    )
  }

  override def classifyEdges(): Unit = {
    super.classifyEdges()
    edges.classed(__CSS_READ_EDGE__, (edge: Edge) => edge.source.isInstanceOf[AddrNode] && edge.target.isInstanceOf[CmpNode])
  }
}

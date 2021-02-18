package maf.web

object AddressVisualisationIncremental {
  val __CSS_DELADDR_NODE__ = "deladdr_node" // Deleted address node.
  val __CSS_WRITE_EDGE__ = "write_edge"
  val __CSS_DELREAD_EDGE__ = "del_read" // Deleted read.
  val __CSS_DELWRITE_EDGE__ = "del_write"
}

trait AddressVisualisationIncremental extends WebVisualisationIncremental with AddressVisualisation {

  // Now also add edges for writes.
  override def refreshData(): Unit = {
    super.refreshData()
    nodesData.foreach { case addrNode: AddrNode =>
    }
  }
}

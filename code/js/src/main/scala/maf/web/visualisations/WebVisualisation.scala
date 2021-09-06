package maf.web.visualisations

import maf.core._
import maf.modular._
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util.benchmarks.Timeout

import maf.web.utils.JSHelpers._
import maf.web.utils.D3Helpers._

// Scala.js-related imports
import scala.scalajs.js
import org.scalajs.dom.document

// null values are used here due to JS interop
import scala.language.unsafeNulls

trait WebVisualisationAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr] with DependencyTracking[Expr]:

    type Module
    def module(cmp: Component): Module
    def moduleName(module: Module): String

    var webvis: WebVisualisation = _

    override def intraAnalysis(component: Component): IntraAnalysis with DependencyTrackingIntra

    override def step(timeout: Timeout.T): Unit =
        webvis.beforeStep()
        super.step(timeout)
        webvis.afterStep()

object WebVisualisation:
    // some constants
    val __CIRCLE_RADIUS__ = 15
    val __SVG_ARROW_ID__ = "endarrow"
    val __CSS_NOT_VISITED__ = "not_visited"
    val __CSS_IN_WORKLIST__ = "in_worklist"
    val __CSS_NEXT_COMPONENT__ = "next_component"
    val __FORCE_COLLIDE__ = "collide"
    val __FORCE_CHARGE__ = "charge"
    val __FORCE_LINKS__ = "links"
    val __FORCE_CENTER__ = "center"

abstract class WebVisualisation(width: Int, height: Int):

    import WebVisualisation._

    val analysis: WebVisualisationAnalysis[_]

    // give the analysis a pointer to this webvis
    analysis.webvis = this

    // TODO: make these abstract
    def componentText(cmp: analysis.Component): String = cmp.toString
    def componentKey(cmp: analysis.Component): Any = None

    //
    // COLOR WHEEL
    //

    var colorWheel: Map[Any, JsAny] = Map()
    def colorFor(cmp: analysis.Component): JsAny = colorForKey(componentKey(cmp))
    def colorForKey(key: Any): JsAny = colorWheel.get(key) match
        case None =>
          val newColor = randomColor()
          colorWheel += (key -> newColor)
          newColor
        case Some(existingColor) => existingColor

    //
    // BOOKKEEPING (needed to play nice with Scala.js and d3.js)
    //

    var edgeID: Int = 0 // Last unused edge id.
    def newEdgeID(): String =
        val id = s"edge$edgeID"
        edgeID += 1
        id

    // TODO: find a better interface
    trait Node extends js.Object:
        def displayText(): String
        def data(): Any

    class CmpNode(val component: analysis.Component) extends Node:
        def displayText(): String = componentText(component)
        def data(): Any = component

    // An edge contains an id so it can be referenced (e.g., to add text).
    class Edge(val source: Node, val target: Node) extends js.Object:
        val id: String = newEdgeID() // Linking errors arise when adding this as a class argument...

    var nodesData: Set[Node] = Set()
    var edgesData: Set[Edge] = Set()
    // TODO: use weak maps here to prevent memory leaks?
    var nodesColl: Map[analysis.Component, CmpNode] = Map()
    var edgesColl: Map[(Node, Node), Edge] = Map()

    def getNode(cmp: analysis.Component): CmpNode = nodesColl.get(cmp) match
        case None =>
          val newNode = new CmpNode(cmp)
          nodesColl += (cmp -> newNode)
          newNode
        case Some(existingNode) => existingNode

    def getEdge(source: Node, target: Node): Edge = edgesColl.get((source, target)) match
        case None =>
          val newEdge = new Edge(source, target)
          edgesColl += ((source, target) -> newEdge)
          newEdge
        case Some(existingEdge) => existingEdge

    //
    // VISUALISATION SETUP
    //

    // setup the svg and visualisation skeleton
    val node = document.createElement("div")
    protected val svgDiv = document.createElement("div")
    node.appendChild(svgDiv)
    protected val svgNode = document.createElementNS("http://www.w3.org/2000/svg", "svg")
    svgDiv.appendChild(svgNode)
    protected val svg = d3.select(svgNode).attr("width", width).attr("height", height)
    private val outerContainer = svg.append("g")
    private val innerContainer = outerContainer.append("g").attr("transform", s"translate(${width / 2},${height / 2})")
    // augment the svg capabilities
    setupMarker(svg) // <- this allows us to use a fancy arrow in the svg
    svg.call(
      d3.zoom()
        .on("zoom",
            () => { // <- this sets up a fancy zoom effect
              outerContainer.attr("transform", d3.event.transform)
            }
        )
    )
    // setup the nodes infrastructure
    private val nodesContainer = innerContainer.append("g").attr("class", "nodes")
    protected var nodes = nodesContainer.selectAll("g")
    // setup the edges infrastructure
    private val edgesContainer = innerContainer.append("g").attr("class", "links")
    protected var edges = edgesContainer.selectAll("path")
    // setup the labels infrastructure
    private val labelsContainer = innerContainer.append("g").attr("class", "labels")
    protected var labels = labelsContainer.selectAll("label")
    // setup the simulation
    private val simulation = d3.forceSimulation()
    simulation
      .force(__FORCE_COLLIDE__, d3.forceCollide().radius(__CIRCLE_RADIUS__))
      .force(__FORCE_CHARGE__, d3.forceManyBody().strength(-500))
      .force(__FORCE_LINKS__, d3.forceLink().distance(150))
      .force(__FORCE_CENTER__, d3.forceCenter())
      .on("tick", () => onTick())

    // Adds a new base marker to the given svg. The marker is returned, so extra attributes can be added later.
    protected def newMarker(svg: JsAny, id: String) =
        // adapted from http://bl.ocks.org/fancellu/2c782394602a93921faff74e594d1bb1
        val marker: js.Dynamic = svg
          .append("defs")
          .append("marker")
          .attr("id", id)
          .attr("viewBox", "-0 -5 10 10")
          .attr("refX", 0)
          .attr("refY", 0)
          .attr("orient", "auto")
          .attr("markerWidth", 5)
          .attr("markerHeight", 5)
        marker
          .append("svg:path")
          .attr("d", "M 0,-5 L 10 ,0 L 0,5")
        marker

    protected def setupMarker(svg: JsAny) = newMarker(svg, __SVG_ARROW_ID__)

    //.attr("fill", "#999")
    //.style("stroke","none")

    def onTickHook(): Unit = ()

    private def onTick() =
        // update the nodes
        nodes.attr("transform", (node: JsAny) => s"translate(${node.x},${node.y})")
        // update the edges
        edges.attr(
          "d",
          (edge: JsAny) =>
            if edge.source == edge.target then {
              val cx = edge.source.x.asInstanceOf[Double]
              val cy = edge.source.y.asInstanceOf[Double]
              val x1 = cx - __CIRCLE_RADIUS__
              val y1 = cy
              val x2 = cx - 9
              val y2 = cy - __CIRCLE_RADIUS__ - 8
              s"M$x1 $y1 A ${__CIRCLE_RADIUS__} ${__CIRCLE_RADIUS__} 1 1 1 $x2 $y2"
            } else {
              val sourceX = edge.source.x.asInstanceOf[Double]
              val sourceY = edge.source.y.asInstanceOf[Double]
              val targetX = edge.target.x.asInstanceOf[Double]
              val targetY = edge.target.y.asInstanceOf[Double]
              val deltaX = targetX - sourceX
              val deltaY = targetY - sourceY
              val dist = Math.sqrt((deltaX * deltaX) + (deltaY * deltaY))
              val scaleFactorSource = __CIRCLE_RADIUS__ / dist
              val scaleFactorTarget = (__CIRCLE_RADIUS__ + 10) / dist
              val x1 = sourceX + (deltaX * scaleFactorSource)
              val x2 = targetX - (deltaX * scaleFactorTarget)
              val y1 = sourceY + (deltaY * scaleFactorSource)
              val y2 = targetY - (deltaY * scaleFactorTarget)
              s"M$x1 $y1 L$x2 $y2"
            }
        )
        // Maybe perform other updates.
        // TODO: just override the existing onTick method (using super.onTick())?
        onTickHook()

    //
    // REFRESHING
    //

    // updates both the data and the visualisation
    def refresh(): Unit =
        refreshData()
        refreshVisualisation()

    // Ensures that `nodesData` and `edgesData` are in sync with the analysis.
    // Allows deleted components to be removed from the visualiation.
    def refreshData(): Unit =
        // refresh the nodes
        nodesData = Set.empty[Node]
        analysis.visited.foreach { cmp =>
            val node = getNode(cmp)
            nodesData += node
            val targets = analysis.dependencies(cmp)
            targets.foreach { target =>
                val targetNode = getNode(target)
                val edge = getEdge(node, targetNode)
                edgesData += edge
            }
        }

    // More efficient than `refreshData`: updates only data that may have changed after stepping.
    protected var prevComponent: analysis.Component = _
    private var prevCalls: Set[analysis.Component] = _

    def beforeStep(): Unit =
        prevComponent = analysis.workList.head
        prevCalls = analysis.dependencies(prevComponent)

    def afterStep(): Unit =
        // refresh the data
        refreshDataAfterStep()
        // refresh the visualisation
        refreshVisualisation()

    protected def refreshDataAfterStep(): Unit =
        val sourceNode = getNode(prevComponent)
        prevCalls.foreach { otherCmp =>
            val targetNode = getNode(otherCmp)
            val edge = getEdge(sourceNode, targetNode)
            edgesData -= edge
        }
        // add the new edges
        analysis.dependencies(prevComponent).foreach { otherCmp =>
            val targetNode = getNode(otherCmp)
            val edge = getEdge(sourceNode, targetNode)
            nodesData += targetNode
            edgesData += edge
        }

    // Can be overridden to do perform extra updates upon the refresh of the visualisation.
    // TODO: just override existing method?
    def refreshHook(): Unit = ()

    // updates the visualisation: draws all nodes/edges, sets correct CSS classes, etc.
    def refreshVisualisation(): Unit =
        // update the nodes
        val nodesUpdate = nodes.data(nodesData, (n: Node) => n.data())
        val newGroup = nodesUpdate
          .enter()
          .append("g")
          .call(dragEffect)
        newGroup
          .append("circle")
          .attr("r", __CIRCLE_RADIUS__)
        newGroup
          .append("text")
          .attr("dx", __CIRCLE_RADIUS__)
          .attr("dy", __CIRCLE_RADIUS__)
        nodes = newGroup.merge(nodesUpdate)
        nodes
          .select("text")
          .text((node: Node) => node.displayText())
        nodesUpdate.exit().remove()
        classifyNodes()
        // update the edges
        val edgesUpdate = edges.data(edgesData, (e: Edge) => (e.source.data(), e.target.data()))
        edges = edgesUpdate.enter().append("path").merge(edgesUpdate)
        edges.attr("id", (e: Edge) => e.id)
        classifyEdges()
        edgesUpdate.exit().remove()
        // possibly perform more updates
        refreshHook()
        classifyLabels()
        // update the simulation
        simulation.nodes(nodesData)
        simulation.force(__FORCE_LINKS__).links(edgesData)
        simulation.alpha(1).restart()

    /** Classifies every node based on its role in the analysis, so the node can be coloured correctly. */
    def classifyNodes(): Unit =
      nodes
        // Apparently the Scala compiler does not just accept the cases as anonymous function, hence the longer implementation.
        .classed(__CSS_IN_WORKLIST__,
                 (node: Node) =>
                   node match {
                     case node: CmpNode => analysis.workList.toSet.contains(node.component)
                     case _             => false
                   }
        )
        .classed(__CSS_NOT_VISITED__,
                 (node: Node) =>
                   node match {
                     case node: CmpNode => !analysis.visited.contains(node.component)
                     case _             => false
                   }
        )
        .classed(__CSS_NEXT_COMPONENT__,
                 (node: Node) =>
                   node match {
                     case node: CmpNode => analysis.workList.toList.headOption.contains(node.component)
                     case _             => false
                   }
        )

    //.style("fill", (node: Node) => colorFor(node.component))

    /** Classifies every edge based on its role in the analysis, so the edge can be coloured correctly. */
    def classifyEdges(): Unit = ()
    def classifyLabels(): Unit = ()

    //
    // DRAGGING
    //

    // create a fancy drag effect
    val dragEffect = d3
      .drag()
      .on("start", (node: JsAny) => onDragStart(node))
      .on("drag", (node: JsAny) => onDragDrag(node))
      .on("end", (node: JsAny) => onDragEnd(node))

    private def onDragStart(node: JsAny): Unit =
        val isActive = d3.event.active.asInstanceOf[Int]
        if isActive == 0 then simulation.alphaTarget(0.3).restart()
        node.fx = node.x
        node.fy = node.y
    private def onDragDrag(node: JsAny): Unit =
        node.fx = d3.event.x
        node.fy = d3.event.y
    private def onDragEnd(node: JsAny): Unit =
        val isActive = d3.event.active.asInstanceOf[Int]
        if isActive == 0 then simulation.alphaTarget(0)
        node.fx = null
        node.fy = null

    refresh()

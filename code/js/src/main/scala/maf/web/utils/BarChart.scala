package maf.web.utils

// Scala.js-related imports
import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js
import js.DynamicImplicits.number2dynamic

import maf.web.utils.D3Helpers._
import maf.web.utils.JSHelpers._

abstract class BarChart(
    val width: Int,
    val height: Int,
    val padding: Int = 100,
    barWidth: Int = 50):

    // visualises of a certain kind of data ...
    type Data
    // ... which should have a key ...
    def key(d: Data): String
    // ... and a (numerical) value
    def value(d: Data): Int

    //
    // Setup the outer DOM element
    //

    val node = document.createElement("div").asInstanceOf[html.Div]
    d3.select(node)
      .style("width", s"${width}px")
      .style("height", s"${height}px")
      .style("overflow-x", "scroll")
      .style("overflow-y", "hidden")

    //
    // Setup the skeleton for the bar chart
    //

    private val realHeight = height - 2 * padding

    protected val svgNode = d3
      .select(node)
      .append("svg")
      .style("height", s"${height}px")
    protected val innerNode = svgNode
      .append("g")
      .attr("transform", s"translate($padding, $padding)")

    private val xScale = d3.scaleBand().padding(0.25)
    private val yScale = d3.scaleLinear().domain(Seq(0, 100)).range(Seq(realHeight, 0))
    private val xAxis = d3.axisBottom(xScale)
    private val yAxis = d3.axisLeft(yScale).ticks(1).tickFormat(d3.format(",.0f"))

    private val xAxisNode = innerNode
      .append("g")
      .attr("transform", s"translate(0, $realHeight)")
    private val yAxisNode = innerNode.append("g")

    // TODO: parameterise this using an overridable method `setupYAxis(max: Int)`
    private var currentMax = 1
    private def increaseMax(max: Int) =
        while currentMax < max do { currentMax *= 2 }
        yScale.domain(Seq(0, currentMax))
        yAxis.ticks(Math.min(10, currentMax))

    // convience method to give the enclosing SVG a certain class
    def classed(className: String) = svgNode.classed(className, true)

    // can be overriden for custom behaviour when clicking an element
    protected def onClick(d: Data): Unit = ()

    // handlers for mouse hovering
    protected def onMouseOver(node: dom.Node, data: Data) =
      d3.select(node).classed("hovered", true)
    protected def onMouseMove(node: dom.Node, data: Data) = ()
    protected def onMouseLeave(node: dom.Node, data: Data) =
      d3.select(node).classed("hovered", false)

    def loadData(data: Iterable[Data]): Unit =

        val n = data.length
        val realWidth = n * barWidth

        // rescale the svg
        val svgWidth = Math.max(realWidth + 2 * padding, width - 2 * padding)
        svgNode.style("width", s"${svgWidth}px")

        // setup the x-axis
        xScale
          .domain(data.map(key))
          .range(Seq(0, realWidth))
        xAxisNode
          .call(xAxis)
          .selectAll("text") // select all text labels of the axis ...
          .attr("transform", "translate(-10,0)rotate(-45)")
          .style("text-anchor", "end")

        // setup the y-axis
        if data.nonEmpty then { increaseMax(value(data.maxBy(value))) }
        yAxisNode.call(yAxis)

        // draw the bars
        val selection = innerNode.selectAll(".bar").data(data, (d: Data) => key(d))
        val enter = selection
          .enter()
          .append("g")
          .attr("class", "bar")
          .on("click", (d: Data) => onClick(d))
          .on("mouseover", { (jsthis: dom.Node, data: Data) => onMouseOver(jsthis, data) }: js.ThisFunction)
          .on("mousemove", { (jsthis: dom.Node, data: Data) => onMouseMove(jsthis, data) }: js.ThisFunction)
          .on("mouseleave", { (jsthis: dom.Node, data: Data) => onMouseLeave(jsthis, data) }: js.ThisFunction)
        // add a rectangle + value label for every new bar
        enter
          .append("text")
          .style("text-anchor", "middle")
          .attr("dy", -8)
        enter
          .append("rect")
        // update existing bars
        val all = enter.merge(selection.transition())
        all.attr("transform", (d: Data) => s"translate(${xScale(key(d))}, ${yScale(value(d))})")
        all
          .select("text")
          .text((d: Data) => value(d).toString)
          .attr("dx", (d: Data) => xScale.bandwidth() / 2)
        all
          .select("rect")
          .attr("width", xScale.bandwidth())
          .attr("height", (d: Data) => realHeight.asInstanceOf[Double] - yScale(value(d)))
        selection.exit().remove()

    // convencience method: arrange bars in descending order
    def loadDataSorted(data: Iterable[Data]) =
      loadData(data.toList.sortBy(value)(Ordering[Int].reverse))

//
// trait for adding a tooltip to the bar
//

trait BarChartTooltip extends BarChart:

    // should be implemented to determine corresponding text in the tooltip
    protected def tooltipText(d: Data): String

    override protected def onMouseOver(node: dom.Node, data: Data) =
        super.onMouseOver(node, data)
        tooltip.html(tooltipText(data)) // TODO: escape some chars for inline HTML
        tooltip.style("visibility", "visible")

    override protected def onMouseMove(node: dom.Node, data: Data) =
        super.onMouseMove(node, data)
        tooltip
          .style("left", s"${d3.event.pageX + 20}px")
          .style("top", s"${d3.event.pageY}px")

    override protected def onMouseLeave(node: dom.Node, data: Data) =
        super.onMouseLeave(node, data)
        tooltip.style("visibility", "hidden")

    // TODO: some of these fixed constants might want to be parameterised
    lazy val tooltip = d3
      .select(node)
      .append("div")
      .attr("class", "tooltip")
      .style("position", "absolute")
      .style("visibility", "hidden")
      .style("background-color", "white")
      .style("border", "solid")
      .style("border-width", "2px")
      .style("border-radius", "5px")
      .style("padding", "5px")

//
// trait for allowing focusing on bars in the barchart
//

trait BarChartFocus extends BarChart:

    var focused: Boolean = false
    def focus(data: Data): Unit = focus(_ == data)
    def focus(included: Data => Boolean): Unit =
        innerNode
          .selectAll(".bar")
          .classed("focused", (d: Data) => included(d))
          .classed("unfocused", (d: Data) => !included(d))
        focused = true
    def resetFocus() =
      if focused then
          innerNode
            .selectAll(".bar")
            .classed("focused", false)
            .classed("unfocused", false)
          focused = false

//
// trait for adding simple stats (total, average) to the top of the bar chart
//

trait BarChartStats extends BarChart:
    // can be overriden for custom behaviour when clicking on / hovering over the labels
    protected def onTotalClick(node: dom.Node) = ()
    protected def onTotalMouseOver(node: dom.Node) = d3.select(node).classed("hovered", true)
    protected def onTotalMouseMove(node: dom.Node) = ()
    protected def onTotalMouseLeave(node: dom.Node) = d3.select(node).classed("hovered", false)
    protected def onAverageClick(node: dom.Node) = ()
    protected def onAverageMouseOver(node: dom.Node) = d3.select(node).classed("hovered", true)
    protected def onAverageMouseMove(node: dom.Node) = ()
    protected def onAverageMouseLeave(node: dom.Node) = d3.select(node).classed("hovered", false)
    // add some information on top
    protected val totalText =
      innerNode
        .append("text")
        .attr("class", "info")
        .attr("x", 20)
        .attr("y", -20)
        .on("click", { (jsthis: dom.Node) => onTotalClick(jsthis) }: js.ThisFunction)
        .on("mouseover", { (jsthis: dom.Node) => onTotalMouseOver(jsthis) }: js.ThisFunction)
        .on("mousemove", { (jsthis: dom.Node) => onTotalMouseMove(jsthis) }: js.ThisFunction)
        .on("mouseleave", { (jsthis: dom.Node) => onTotalMouseLeave(jsthis) }: js.ThisFunction)
    protected val averageText =
      innerNode
        .append("text")
        .attr("class", "info")
        .attr("x", (width - 2 * padding) / 2)
        .attr("y", -20)
        .on("click", { (jsthis: dom.Node) => onAverageClick(jsthis) }: js.ThisFunction)
        .on("mouseover", { (jsthis: dom.Node) => onAverageMouseOver(jsthis) }: js.ThisFunction)
        .on("mousemove", { (jsthis: dom.Node) => onAverageMouseMove(jsthis) }: js.ThisFunction)
        .on("mouseleave", { (jsthis: dom.Node) => onAverageMouseLeave(jsthis) }: js.ThisFunction)
    // update the labels when loading in new data
    override def loadData(data: Iterable[Data]) =
        // load the data as usual
        super.loadData(data)
        // set the length text
        val length = data.size
        totalText.text(s"total: $length")
        // set the average text
        if length == 0 then averageText.text("average: N/A")
        else
            val average = data.map(value).sum / length
            averageText.text(s"average: $average")

//
// convenience class for barcharts with simple data
//

class SimpleBarChart(width: Int, height: Int) extends BarChart(width, height):
    type Data = (_, Int)
    def key(d: Data) = d._1.toString
    def value(d: Data) = d._2

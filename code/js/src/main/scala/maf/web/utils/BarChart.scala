package maf.web.utils

// Scala.js-related imports
import org.scalajs.dom._
import scala.scalajs.js
import js.DynamicImplicits.number2dynamic

import maf.web.utils.D3Helpers._
import maf.web.utils.JSHelpers._

abstract class BarChart(
    width: Int,
    height: Int,
    padding: Int = 100,
    barWidth: Int = 50,
    barClass: String = "bar") {

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

  //
  // Setup the skeleton for the bar chart
  //

  private val realHeight = height - 2 * padding

  private val svgNode = d3
    .select(node)
    .append("svg")
    .style("height", s"${height}px")
  private val innerNode = svgNode
    .append("g")
    .attr("transform", s"translate($padding, $padding)")

  private val xScale = d3.scaleBand().padding(0.25)
  private val yScale = d3.scaleLinear().domain(Seq(0, 100)).range(Seq(realHeight, 0))
  private val xAxis = d3.axisBottom(xScale)
  private val yAxis = d3.axisLeft(yScale).ticks(10)

  private val xAxisNode = innerNode
    .append("g")
    .attr("transform", s"translate(0, $realHeight)")
  private val yAxisNode = innerNode.append("g")

  // TODO: parameterise this using an overridable method `setupYScale(max: Int)`
  private var currentMax = 1
  private def increaseMax(max: Int) = {
    while (currentMax < max) { currentMax *= 2 }
    yScale.domain(Seq(0, currentMax))
  }

  // can be override for custom behaviour when clicking an element
  protected def onClick(d: Data): Unit = ()

  def loadData(data: Iterable[Data]): Unit = {

    val n = data.length
    val realWidth = n * barWidth

    // rescale the svg
    svgNode.style("width", s"${realWidth + 2 * padding}px")

    // setup the x-axis
    xScale
      .domain(data.map(key))
      .range(Seq(0, realWidth))
    xAxisNode
      .call(xAxis)
      .selectAll("text") // select all text labels of the axis ...
      .attr("y", 0)
      .attr("x", 9)
      .attr("dy", ".35em")
      .attr("transform", "rotate(90)") // ... and rotate them by 90 degrees
      .style("text-anchor", "start")

    // setup the y-axis
    if (data.nonEmpty) { increaseMax(value(data.maxBy(value))) }
    yAxisNode.call(yAxis)

    // draw the bars
    val selection = innerNode.selectAll(s".$barClass").data(data, (d: Data) => key(d))
    selection
      .enter()
      .append("rect")
      .attr("class", barClass)
      .attr("width", xScale.bandwidth())
      .on("click", (d: Data) => onClick(d))
      .merge(selection.transition())
      .attr("x", (d: Data) => xScale(key(d)))
      .attr("y", (d: Data) => yScale(value(d)))
      .attr("height", (d: Data) => realHeight - yScale(value(d)))
    selection.exit().remove()
  }

  // convencience method: arrange bars in descending order
  def loadDataSorted(data: Iterable[Data]) =
    loadData(data.toList.sortBy(value)(Ordering[Int].reverse))
}

class SimpleBarChart(width: Int, height: Int) extends BarChart(width, height) {
  type Data = (_, Int)
  def key(d: Data) = d._1.toString
  def value(d: Data) = d._2
}

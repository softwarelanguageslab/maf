package maf.web.utils

import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLHtmlElement

import scala.scalajs.js
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLLinkElement

trait ColumnExtract[T]:
    def extract(v: T): List[String]

object ColumnExtract:
    def apply[T: ColumnExtract]: ColumnExtract[T] = summon[ColumnExtract[T]]

    given tuple2[A, B]: ColumnExtract[(A, B)] with
        def extract(v: (A, B)): List[String] =
            List(v._1.toString, v._2.toString)

class HtmlTable[T: ColumnExtract](header: List[String]):
    /** Initially no table has been rendered */
    private var renderedTable: Option[HTMLElement] = None

    /** A map of data points to rows in the table */
    private var renderedRows: Map[T, HTMLElement] = Map()

    /** Can be called to compute the class of the rows in the table */
    def classed(f: PartialFunction[T, String]): Unit =
        renderedRows.keySet.foreach(data =>
            if f.isDefinedAt(data) then
                val cls = f.apply(data)
                renderedRows(data).className = cls
        )

    def addRow(data: T, cls: Option[String] = None): Unit =
        if !renderedRows.contains(data) then
            val columns = ColumnExtract[T].extract(data)
            val renderedRow = document.createElement("tr").asInstanceOf[HTMLElement]
            columns.foreach(col =>
                val renderedCol = document.createElement("td").asInstanceOf[HTMLElement]
                renderedCol.innerText = col
                renderedRow.appendChild(renderedCol)
            )
            renderedRows = renderedRows + (data -> renderedRow)

            if cls.isDefined then renderedRow.className = cls.get
            renderedTable.get.appendChild(renderedRow)

    def update(old: T, nww: T, cls: Option[String]): Unit =
        val oldRender = renderedRows(old)
        oldRender.innerHTML = ""
        val columns = ColumnExtract[T].extract(nww)
        columns.foreach(col =>
            val renderedCol = document.createElement("td")
            renderedCol.innerText = col
            oldRender.appendChild(renderedCol)
        )
        if cls.isDefined then oldRender.className = cls.get
        renderedRows = renderedRows - old
        renderedRows = renderedRows + (nww -> oldRender)

    def render(container: HTMLElement): Unit = renderedTable match
        case Some(t) =>
            container.removeChild(t) // no-op if element is not there
            container.appendChild(t)
        case _ =>
            val table = document.createElement("table").asInstanceOf[HTMLLinkElement]
            val headerRow = document.createElement("tr")
            header.foreach(name =>
                val headerColumn = document.createElement("td")
                headerColumn.innerText = name
                headerRow.appendChild(headerColumn)
            )
            table.appendChild(headerRow)
            container.appendChild(table)
            renderedTable = Some(table)

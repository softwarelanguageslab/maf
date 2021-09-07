package maf.web.utils

// Scala.js-related imports
import org.scalajs.dom
import org.scalajs.dom._
import D3Helpers.d3

object Div:

    trait Arrangement
    case object Horizontal extends Arrangement
    case object Vertical extends Arrangement

    def apply(arrangement: Arrangement)(children: dom.Node*): html.Div =
        val div = document.createElement("div").asInstanceOf[html.Div]
        d3.select(div).style("overflow", "hidden")
        children.foreach { child =>
            arrangement match
                case Horizontal => d3.select(child).style("float", "left")
                case Vertical   => () // default is apparently vertical anyway in CSS
            div.appendChild(child)
        }
        return div

// for convenience
object HStack:
    def apply(children: dom.Node*): html.Div =
      Div(Div.Horizontal)(children: _*)
object VStack:
    def apply(children: dom.Node*): html.Div =
      Div(Div.Vertical)(children: _*)

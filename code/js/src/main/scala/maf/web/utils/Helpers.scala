package maf.web.utils

import scala.scalajs.js

object JSHelpers:
    // shorthands
    type JsAny = js.Dynamic
    type JsArray[E] = js.Array[E]
    // more helpers
    implicit def toJsArray[E](seq: Iterable[E]): JsArray[E] =
        val array = new js.Array[E]()
        seq.foreach(item => array.push(item))
        return array

object D3Helpers:
    // shorthands
    val d3: JSHelpers.JsAny = js.Dynamic.global.d3
    // more helpers
    def randomColor(): JSHelpers.JsAny =
        val r = (scala.math.random() * 255).toInt
        val g = (scala.math.random() * 255).toInt
        val b = (scala.math.random() * 255).toInt
        d3.rgb(r, g, b)

    def rgb(r: Int, g: Int, b: Int): JSHelpers.JsAny = d3.rgb(r, g, b)

    val RED = rgb(255, 0, 0)
    val GREEN = rgb(0, 255, 0)
    val BLUE = rgb(0, 0, 255)

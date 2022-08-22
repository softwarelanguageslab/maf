package maf.util.graph

case class Color(hex: String):
    override def toString = hex
object Colors:
    object Black extends Color("#000000")
    object Blue extends Color("#DDFFFF")
    object BlueGreen extends Color("#40E0D0")
    object DarkBlue extends Color("#00008B")
    object Grass extends Color("#00FF00")
    object Green extends Color("#DDFFDD")
    object Grey extends Color("#CCCCCC")
    object Pink extends Color("#FFDDDD")
    object PinkOrange extends Color("#FE7968")
    object Red extends Color("#FF0000")
    object White extends Color("#FFFFFF")
    object Yellow extends Color("#FFFFDD")
    lazy val allColors = List(Green, Yellow, Grass, Pink, Blue, Red, BlueGreen, Grey, DarkBlue, PinkOrange, Black, White)

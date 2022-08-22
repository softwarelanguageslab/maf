package maf.util.graph

case class Color(hex: String):
    override def toString: String = hex
object Colors:
    object BabyBlue extends Color("#9FC2E2")
    object Black extends Color("#000000")
    object Blue extends Color("#DDFFFF")
    object BlueGreen extends Color("#40E0D0")
    object BrightYellow extends Color("#DFFF00")
    object DarkBlue extends Color("#00008B")
    object DullOrange extends Color("#E2BF9F")
    object Grass extends Color("#00FF00")
    object Green extends Color("#DDFFDD")
    object Grey extends Color("#CCCCCC")
    object Olive extends Color("#C2E29F")
    object Pink extends Color("#FFDDDD")
    object PinkOrange extends Color("#FE7968")
    object Red extends Color("#FF0000")
    object SoftGreen extends Color("#9FE2BF")
    object White extends Color("#FFFFFF")
    object Yellow extends Color("#FFFFDD")
    lazy val allColors: List[Color] = List(Green, Yellow, BabyBlue, Grass, Pink, BrightYellow, Blue, SoftGreen, Red, BlueGreen, DullOrange, DarkBlue, Olive, PinkOrange, Grey, Black, White)

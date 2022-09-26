package maf.util.graph

case class Color(hex: String):
    override def toString: String = hex
object Colors:
    object BabyBlue extends Color("#9FC2E2")
    object Black extends Color("#000000")
    object Blue extends Color("#DDFFFF") // Soft blue
    object BlueGreen extends Color("#40E0D0")
    object BrightYellow extends Color("#DFFF00") // Fluo
    object DarkBlue extends Color("#00008B")
    object DullOrange extends Color("#E2BF9F")
    object Grass extends Color("#00FF00") // Fluo
    object Green extends Color("#DDFFDD") // Soft green
    object Grey extends Color("#CCCCCC")
    object Olive extends Color("#C2E29F")
    object Pink extends Color("#FFDDDD")
    object PinkOrange extends Color("#FE7968") // Salmon
    object Red extends Color("#FF0000")
    object SoftGreen extends Color("#9FE2BF")
    object White extends Color("#FFFFFF")
    object Yellow extends Color("#FFFFDD") // Soft yellow
    lazy val softColors: List[Color] = List(BabyBlue, DullOrange, Green, Yellow, Blue, PinkOrange, Olive, Pink, SoftGreen)
    lazy val brightColors: List[Color] = List(BlueGreen, BrightYellow, Grass, Red)
    lazy val darkColors: List[Color] = List(DarkBlue)
    lazy val greyScale: List[Color] = List(Grey, Black, White)
    lazy val palette: List[Color] = softColors ++ brightColors ++ darkColors ++ greyScale // Some nicer ordering of colours.
    lazy val allColors: List[Color] = List(Green,
                                           Yellow,
                                           BabyBlue,
                                           Grass,
                                           Pink,
                                           BrightYellow,
                                           Blue,
                                           SoftGreen,
                                           Red,
                                           BlueGreen,
                                           DullOrange,
                                           DarkBlue,
                                           Olive,
                                           PinkOrange,
                                           Grey,
                                           Black,
                                           White
    )

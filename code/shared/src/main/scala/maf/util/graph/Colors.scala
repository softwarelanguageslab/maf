package maf.util.graph

case class Color(hex: String):
    override def toString: String = hex
object Colors:
    object BabyBlue extends Color("#9FC2E2")
    object Beige extends Color("#999966")
    object Black extends Color("#000000")
    object Blue extends Color("#DDFFFF") // Soft blue
    object BlueGreen extends Color("#40E0D0")
    object Bordeaux extends Color("#990000")
    object BrightBlue extends Color("#00FFFF")
    object BrightPink extends Color("#FF0066")
    object BrightYellow extends Color("#DFFF00") // Fluo
    object Brown extends Color("#996633")
    object DarkBlue extends Color("#00008B")
    object DarkGreen extends Color("#008000")
    object DarkGrey extends Color("#999999")
    object DarkOlive extends Color("#C0CAAD")
    object DullOrange extends Color("#E2BF9F")
    object Grass extends Color("#00FF00") // Fluo
    object Green extends Color("#DDFFDD") // Soft green
    object GreenishGrey extends Color("#9DA9A0")
    object Grey extends Color("#CCCCCC")
    object Ocre extends Color("#DFAF2C")
    object Olive extends Color("#C2E29F")
    object PaleViolet extends Color("#CC99FF")
    object Pink extends Color("#FFDDDD")
    object PinkOrange extends Color("#FE7968") // Salmon
    object Red extends Color("#FF0000")
    object Redwood extends Color("#C0CAAD")
    object RoseEbony extends Color("#654C4F")
    object SoftGreen extends Color("#9FE2BF")
    object Violet extends Color("#8806CE")
    object White extends Color("#FFFFFF")
    object Yellow extends Color("#FFFFDD") // Soft yellow
    lazy val softColors: List[Color] = List(BabyBlue, DullOrange, Green, Yellow, Blue, PinkOrange, Olive, Pink, SoftGreen, PaleViolet, DarkOlive, Redwood, GreenishGrey)
    lazy val brightColors: List[Color] = List(BlueGreen, BrightYellow, Grass, Red, BrightBlue, BrightPink)
    lazy val darkColors: List[Color] = List(DarkBlue, DarkGreen, Ocre, Bordeaux, Violet, RoseEbony, Beige, Brown)
    lazy val greyScale: List[Color] = List(Grey, DarkGrey, Black, White)
    lazy val palette: List[Color] = softColors ++ brightColors ++ darkColors ++ greyScale // Some nicer ordering of colours.
    /* lazy val allColors: List[Color] = List(Green,
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
    ) */

package maf.util
import scala.io.AnsiColor.*

/**
 * Wrapper for io.AnsiColor.
 */
object ColouredFormatting {
    // Wrapper for easily accessing https://www.scala-lang.org/api/2.13.5/scala/io/AnsiColor.html

    def redText(string: String): String = s"${RESET}${RED}$string${RESET}" // Red text.
    def redBText(string: String): String = s"${RESET}${RED_B}$string${RESET}" // Red background.

    def yellowText(string: String): String = s"${RESET}${YELLOW}$string${RESET}" // Yellow text.
    def yellowBText(string: String): String = s"${RESET}${YELLOW_B}$string${RESET}" // Yellow background.

    def greenText(string: String): String = s"${RESET}${GREEN}$string${RESET}" // Green text.
    def greenBText(string: String): String = s"${RESET}${GREEN_B}$string${RESET}" // Green background.

    def magentaText(string: String): String = s"${RESET}${MAGENTA}$string${RESET}" // Magenta text.
    def magentaBText(string: String): String = s"${RESET}${MAGENTA_B}$string${RESET}" // Magenta background.

    def cyanText(string: String): String = s"${RESET}${CYAN}$string${RESET}" // Cyan text.
    def cyanBText(string: String): String = s"${RESET}${CYAN_B}$string${RESET}" // Cyan background.

    def blueText(string: String) = s"${RESET}${BLUE}$string${RESET}" // Blue text.
    def blueBText(string: String) = s"${RESET}${BLUE_B}$string${RESET}" // Blue background.

    // Aliases
    def markError: String => String = redText
    def markBad: String => String = redBText
    def markWarning: String => String = yellowText
    def markOK: String => String = greenText
    def markInfo: String => String = magentaText
    def markHeader: String => String = cyanBText
    def markTask: String => String = blueBText
    def markStep: String => String = blueText
}
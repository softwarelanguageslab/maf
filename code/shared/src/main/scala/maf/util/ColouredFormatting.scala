package maf.util
import io.AnsiColor.*

object ColouredFormatting {
    // Wrapper for easily accessing https://www.scala-lang.org/api/2.13.5/scala/io/AnsiColor.html

    def markError(string: String): String = s"${RESET}${RED}$string${RESET}" // Red text.
    def markBad(string: String): String = s"${RESET}${RED_B}$string${RESET}" // Red background.
    def markWarning(string: String): String = s"${RESET}${YELLOW}$string${RESET}" // Yellow text.
    def markOK(string: String): String = s"${RESET}${GREEN}$string${RESET}" // Green text.
    def markInfo(string: String): String = s"${RESET}${MAGENTA}$string${RESET}" // Magenta text.

    def markHeader(string: String): String = s"${RESET}${CYAN_B}$string${RESET}" // Cyan background.
    def markTask(string: String) = s"${RESET}${BLUE_B}$string${RESET}" // Blue background.
    def markStep(string: String) = s"${RESET}${BLUE}$string${RESET}" // Blue text.
}

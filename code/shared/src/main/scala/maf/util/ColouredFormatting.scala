package maf.util
import io.AnsiColor.*

object ColouredFormatting {
    // Wrapper for easily accessing https://www.scala-lang.org/api/2.13.5/scala/io/AnsiColor.html

    def markError(string: String): String = s"${RESET}${RED}$string${RESET}"
    def markWarning(string: String): String = s"${RESET}${YELLOW}$string${RESET}"

    def markHeader(string: String): String = s"${RESET}${CYAN_B}$string${RESET}"
    def markTask(string: String) = s"${RESET}${BLUE_B}$string${RESET}"
}

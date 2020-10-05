package scalaam.util

class PrettyPrinter {
  /**
   * The indentation level
   */
  private var level: Int = 0

  /**
   * THe current string builder for the pretty printer
   */
  private val builder: StringBuilder = new StringBuilder()

  /**
   * Generate a new line on the output
   */
  def newline(): Unit =
    builder.append('\n')

  /**
   * Generate a tab on the output
   */
  def newtab(): Unit =
    builder.append('\t')

  /**
   * Generates a new line and advances the indentation level
   */
  def newIndent(): Unit = {
    level += 1
    newline()
    (0 to level).foreach { _ =>
      newtab()
    }
  }

  /**
   *  Exit the indentation level
   */
  def exitIndent(): Unit = {
    level -= 1
    (0 to level).foreach { _ =>
      newtab()
    }
  }

  def print(s: String): Unit =
    builder.append(s)

  def render: String = builder.toString()

}

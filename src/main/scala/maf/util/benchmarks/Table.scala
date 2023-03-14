package maf.util.benchmarks

object Table:

    /**
     * Create a new empty table
     * @return
     *   a new empty table
     */
    def empty[V]: Table[V] = Table(Map[(String, String), V](), None)

case class Table[V](data: Map[(String, String), V], default: Option[V]):

    /**
     * Get an element from the table.
     * @param row
     *   The name of the row
     * @param column
     *   The name of the column
     * @return
     *   An optional value, being either:
     *   - None, if the table does not contain an element at the given row and column and no default value is set.
     *   - Some element in the table at the given row and column otherwise (or the default value if it is set, and no element is found)
     */
    def get(row: String, column: String): Option[V] = data.get((row, column)).orElse(default)

    /**
     * Get an element from the table
     * @param row
     *   The name of the row
     * @param column
     *   The name of the column
     * @return
     *   The value in the table at the given row and column, or the default value if no such element is present (throws an exception if the element is
     *   not found and no default value is set)
     */
    def apply(row: String, column: String): V = this.get(row, column).get

    /**
     * Add an element to the table.
     * @param row
     *   The name of the row
     * @param column
     *   The name of the column
     * @param elem
     *   The element to add to the table
     * @return
     *   A new table in which the element has been added
     */
    def add(
        row: String,
        column: String,
        elem: V
      ): Table[V] = Table(data + ((row, column) -> elem), default)

    /**
     * Add a default value that is used when an element is not present in the table.
     * @param v
     *   The default value to be installed
     * @return
     *   A new table with the given default value
     */
    def withDefaultValue(v: V): Table[V] = Table(data, Some(v))

    /**
     * Return all rows of a given table
     * @return
     *   The set of all row names in the table
     */
    def allRows: Set[String] = data.keySet.map(_._1)

    /**
     * Return all columns of a given table
     * @return
     *   The set of all column names in the table
     */
    def allColumns: Set[String] = data.keySet.map(_._2)

    /**
     * Extract the given rows and columns from a table, converting it to a simpler representation (for further processing)
     * @param rows
     *   A list of row names to include (in the given order) (default: all table rows in alphabetical order)
     * @param columns
     *   A list of column names to include (in the given order) (default: all table columns in alphabetical order)
     * @param rowName
     *   A string indicating the meaning of the row names (default: empty string)
     * @param format
     *   A formatting function to transform the table values into string (default: using the toString method)
     * @return
     *   A list of rows, where each row is represented as a list of values, containing (the string representation of) the value at each column
     */
    private def extract(
        rows: List[String],
        columns: List[String],
        rowName: String,
        format: V => String
      ): List[List[String]] =
        val firstRow = rowName :: columns
        val otherRows = rows.foldRight(List[List[String]]()) { (row, restRows) =>
            (row :: columns.map(col => format(this(row, col)))) :: restRows
        }
        firstRow :: otherRows

    /**
     * Generate LaTeX code to produce the table
     * @param rows
     *   A list of row names to include (in the given order) (default: all table rows in alphabetical order)
     * @param columns
     *   A list of column names to include (in the given order) (default: all table columns in alphabetical order)
     * @param rowName
     *   A string indicating the meaning of the row names (default: empty string)
     * @param format
     *   A formatting function to transform the table values into string (default: using the toString method)
     * @return
     *   A string containing the LaTeX code to generate the table
     */
    def toLatexString(
        rows: List[String] = allRows.toList.sorted,
        columns: List[String] = allColumns.toList.sorted,
        rowName: String = "",
        format: V => String = _.toString()
      ): String =
        val headerStr = s"\\begin{table}\n\\center\n\\begin{tabular}{l${"c" * columns.length}}\n\\toprule\n"
        val content = extract(rows, columns, rowName, v => format(v).replace("_", "\\_").nn)
            .map(_.mkString(" & "))
            .mkString("", "\\\\\n", "\\\\\n")
        val footerStr = "\\bottomrule\n\\end{tabular}\n\\end{table}"
        headerStr ++ content ++ footerStr

    /**
     * Generate a CSV representation of the given table
     * @param rows
     *   A list of row names to include (in the given order) (default: all table rows in alphabetical order)
     * @param columns
     *   A list of column names to include (in the given order) (default: all table columns in alphabetical order)
     * @param rowName
     *   A string indicating the meaning of the row names (default: empty string)
     * @param format
     *   A formatting function to transform the table values into string (default: using the toString method)
     * @return
     *   A string containing the CSV representation of the table
     */
    def toCSVString(
        rows: List[String] = allRows.toList.sorted,
        columns: List[String] = allColumns.toList.sorted,
        rowName: String = "",
        format: V => String = _.toString()
      ): String =
        extract(rows, columns, rowName, format)
            .map(_.mkString(","))
            .mkString("\n")

    /**
     * Generate a string formatted as a table (useable for pretty printing)
     * @param rows
     *   A list of row names to include (in the given order) (default: all table rows in alphabetical order)
     * @param columns
     *   A list of column names to include (in the given order) (default: all table columns in alphabetical order)
     * @param rowName
     *   A string indicating the meaning of the row names (default: empty string)
     * @param format
     *   A formatting function to transform the table values into string (default: using the toString method)
     * @return
     *   A "pretty-print-friendly" string representation of the table
     */
    def prettyString(
        rows: List[String] = allRows.toList.sorted,
        columns: List[String] = allColumns.toList.sorted,
        rowName: String = "",
        format: V => String = _.toString()
      ): String =
        val addPadding = (elm: String, len: Int) => elm + (" " * (len - elm.length))
        val content = extract(rows, columns, rowName, format)
        val colLens = content.transpose.map(_.maxBy(_.size).size)
        val rowsStr = content.map(_.zip(colLens).map(addPadding.tupled).mkString("| ", " | ", " |"))
        val seperator = colLens.map("-" * _).mkString("+-", "-+-", "-+")
        ((seperator :: rowsStr.head :: seperator :: rowsStr.tail) :+ seperator).mkString("\n")

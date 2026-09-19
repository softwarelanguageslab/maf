package maf.test.language.sexp

import org.scalatest.prop._
import org.scalatest.propspec._
import maf.language.sexp._
import maf.test.ParserTest

class CommentsTest extends AnyPropSpec with TableDrivenPropertyChecks {
    val lexical = new SExpLexer
    
    val comments = Table(
        ("input", "expected"),
        // Line comments
        ("; standard line comment\n 42", "42"),
        ("#! hash bang line comment\n 42", "42"),
        // Multi-line block comments
        ("#| block comment |# 42", "42"),
        ("#| outer #| inner |# outer |# 42", "42"),
        ("#| multiple \n lines \n inside |# 42", "42"),
        // Datum comments
        ("#; (1 2 3) 42", "42"),
        ("#; #| comment in datum |# (1 2 3) 42", "42"),
        ("#; #(1 2 3) 42", "42"),
        ("#; 'foo 42", "42"),
        ("#; ,@foo 42", "42"),
        // Nested / Stacked datum comments
        ("#; #; 1 2 3", "3"),
        ("#; #; (1 2) 3 4", "4"),
        // Mixed comments
        ("#; 1 #| block |# #; 2 42", "42"),
        ("#; (1 #; 2 3) 42", "42")
    )

    property("SExpLexer should skip comments correctly", ParserTest) {
        forAll(comments) { (s, exp) =>
            val parser = lexical.token
            parser(new scala.util.parsing.input.CharArrayReader(s.toCharArray.nn)) match {
                case lexical.Success(res, next) =>
                    assert(res.chars == exp)
                case res => throw new Exception(s"Parse failure: $res")
            }
        }
    }
}

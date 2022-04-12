package maf.test.language.symbolic

import maf.test.*
import maf.language.symbolic.lattices.*
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicLatticeTests extends AnyFlatSpec:
    import maf.language.symbolic.Symbolic.*

    def shouldWidenTo(a: String, b: String, rest: Set[String], policy: SymbolicWidenPolicy): Unit =
        a should s"widen to $b given {${rest.mkString(",")}} using policy $policy" taggedAs (ScvTest, SymbolicTest) in {
            val aExp = Parser.parse(a).head
            val bExp = Parser.parse(b).head
            val restExps = rest.map(Parser.parse(_).head)
            val result = policy.widen(aExp, restExps)
            assert(result.isomorphic(bExp))
        }

    // Consider the following function:
    // (define (increment x)
    //    (if (= x 0)
    //        x
    //        (+ 1 (increment (- x 1)))))
    // Then the after the first recursive call, we get:
    shouldWidenTo("(+ (+ x 1) 1)", "(+ □ 1)", Set("(+ x 1)"), WidenRecurringPatterns)
    // The second recursive call is this...
    shouldWidenTo("(+ (+ □ 1) 1)", "(+ □ 1)", Set("(+ □ 1)", "(+ x 1)"), WidenRecurringPatterns)
    // or this, which are both stable:
    shouldWidenTo("(+ (+ x 1) 1)", "(+ □ 1)", Set("(+ □ 1)", "(+ x 1)"), WidenRecurringPatterns)

    //
    shouldWidenTo("(+ x0 0)", "(+ x0 0)", Set(), WidenRecurringPatterns)
    // next iteration
    shouldWidenTo("(+ x0 (+ x0 0))", "(+ x0 □)", Set("(+ x0 0)"), WidenRecurringPatterns)
    // next iteration
    shouldWidenTo("(+ x0 (+ x0 □))", "(+ x0 □)", Set("(+ x0 0)", "(+ x0 □)"), WidenRecurringPatterns)

package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.treeDD.transformations.schemeBinding.{BindingDrop, BindingReplace}
import maf.language.scheme.{SchemeBegin, SchemeParser}

class BindingReplaceTest extends AnyFlatSpecTransformations {
  "LetIdentifierReplace" should "replace a lets identifier" in {
    val programText =
      """(begin
        |  (+ 2 2)
        |  (let ((a 10)
        |        (b 100))
        |    b
        |    (+ b a)
        |    (if #t a 99)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps(1)

    val suggestedTrees = BindingReplace.transform(t, letExp).toList

    assertTreeString("(begin (+ 2 2) (let ((a 10)) () (+ () a) (if #t a 99)))", suggestedTrees)

    assertTreeString("(begin (+ 2 2) (let ((a 10)) #f (+ #f a) (if #t a 99)))", suggestedTrees)
  }
}

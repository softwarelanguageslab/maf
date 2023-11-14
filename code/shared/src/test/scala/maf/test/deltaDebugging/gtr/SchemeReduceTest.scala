package maf.test.deltaDebugging.gtr

import maf.core.NoCodeIdentity
import maf.deltaDebugging.treeDD.SchemeReduce
import maf.deltaDebugging.treeDD.transformations.*
import maf.deltaDebugging.treeDD.transformations.generics.{DeleteChildSimple, ReplaceByChild}
import maf.language.scheme.{SchemeBegin, SchemeCodeChange, SchemeExp, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class SchemeReduceTest extends AnyFlatSpec {
  "GTR" should "be able to reduce a program" in {
    val programText: String =
      "(begin " +
        "(if (= x 5) #t #f)" +
        "(begin (+ x 2) (* 100 100) (if #f #f #f)))"

    val t = SchemeParser.parseProgramText(programText).last

    val reduced = SchemeReduce.reduce(t,
      t => t.isInstanceOf[SchemeBegin] && t.asInstanceOf[SchemeBegin].exps.length == 1, identity,
      List(ReplaceByChild, DeleteChildSimple))

    assert(reduced.isInstanceOf[SchemeBegin])
    assert(reduced.asInstanceOf[SchemeBegin].exps.length == 1)
    }

  "GTR" should "be able to reduce program changes" in {

    val oldString: String =
      "(begin " +
        "(define x 1)" +
        "(if (= x 5) #t #f)" +
        "(begin (+ x 2) (* 100 100) (if #f #f #f)))"

    val oldExp = SchemeParser.parseProgramText(oldString).last

    val newString: String =
      "(begin " +
        "(define x 1)" +
        "(if (= x 5) #t #f))"

    val newExp = SchemeParser.parseProgramText(newString).last

    val change: SchemeCodeChange = SchemeCodeChange(
      oldExp, newExp, NoCodeIdentity
    )

    val reducedChange: SchemeExp = SchemeReduce.reduce(
      change,
      e => {
        e match
          case c: SchemeCodeChange =>
            c.old.isInstanceOf[SchemeBegin] &&
              c.nw.isInstanceOf[SchemeBegin] &&
              c.findUndefinedVariables().isEmpty
          case _ => false
      },
      identity,
      TransformationManager.allTransformations,
    )

    assert(reducedChange.size < 10)
  }
}

package maf.test

import org.scalatest.Assertion
import org.scalatest.propspec.AnyPropSpec

trait SchemeR5RSTests extends AnyPropSpec:

    def testExpr(program: String, answer: Any): Assertion

    SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property(s"Primitive in $e is correct.", PrimitiveTest)(testExpr(e, a)) }

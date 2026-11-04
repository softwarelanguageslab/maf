package maf.test

import org.scalatest.propspec.AnyPropSpec

trait SchemeR5RSTests extends AnyPropSpec:

    def testExpr(program: String, answer: Any): Unit

    SchemeR5RSBenchmarks.bench.foreach { case (e, a) => property(s"Primitive in $e is correct.", PrimitiveTest)(testExpr(e, a)) }

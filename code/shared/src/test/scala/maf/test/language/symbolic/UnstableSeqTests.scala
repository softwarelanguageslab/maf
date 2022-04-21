package maf.test.language.symbolic

import org.scalatest.flatspec.AnyFlatSpec

import maf.language.scheme.*
import maf.language.symbolic.Unstable
import maf.test.*

class UnstableSeqTests extends AnyFlatSpec:
    "(+ x0 1), (+ (+ x0 1) 1), (+ (+ (+ x0 1) 1) 1)" should "be an unstable sequence" taggedAs SymbolicTest in {
        val sequence = List(
          "(+ x0 1)",
          "(+ (+ x0 1) 1)",
          "(+ (+ (+ x0 1) 1) 1)"
        ).map(exp => SchemeParser.parse(exp).head)

        assert(Unstable.isUnstable(sequence))
    }

    "(+ x0 1), (+ (+ x0 1) 2), (+ (+ (+ x0 1) 1) 1)" should "not be an unstable sequence" taggedAs SymbolicTest in {
        val sequence = List(
          "(+ x0 1)",
          "(+ (+ x0 1) 2)",
          "(+ (+ (+ x0 1) 1) 1)"
        ).map(exp => SchemeParser.parse(exp).head)

        assert(!Unstable.isUnstable(sequence))
    }

    "(+ x0 1), (+ 2 (+ x0 1)) ,(+ (+ 2 (+ x0 1)) 1)" should "be an unstable sequence" taggedAs SymbolicTest in {
        val sequence = List(
          "(+ x0 1)",
          "(+ 2 (+ x0 1))",
          "(+ (+ 2 (+ x0 1)) 1)",
        ).map(exp => SchemeParser.parse(exp).head)

        assert(Unstable.isUnstable(sequence))
    }

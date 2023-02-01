package maf.deltaDebugging.nonTreeDD

abstract class TestCase[+C <: Change](val changes: List[C])

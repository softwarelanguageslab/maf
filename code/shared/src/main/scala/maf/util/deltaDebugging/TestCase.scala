package maf.util.deltaDebugging

abstract class TestCase[+C <: Change](val changes: List[C])


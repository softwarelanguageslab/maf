package maf.util.DeltaDebugging

abstract class TestCase[+C <: Change](val changes: List[C])


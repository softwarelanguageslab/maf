package dd

abstract class TestCase[+C <: Change](val changes: List[C])


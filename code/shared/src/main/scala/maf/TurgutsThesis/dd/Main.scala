package maf.TurgutsThesis.dd

object Main {
  case class ListChange(s: String, id: Int = ListChange.newID()) extends Change
  object ListChange {
    private var ID = 0
    def newID(): Int = {
      ID = ID + 1
      ID
    }
  }

  case class ListTestCase(override val changes: List[ListChange]) extends TestCase[ListChange](changes)

  val testCasePass: ListTestCase = ListTestCase(List()) //baseline, or trivial test case that passes the test
  val testCaseFail: ListTestCase = ListTestCase(List(
    ListChange("define"),
    ListChange("/"),
    ListChange("+"),
    ListChange("-"),
    ListChange("++"),
    ListChange("--"),
    ListChange("set!"), //set! is considered buggy
    ListChange("let")))


  object ListDeltaDebugger extends DeltaDebugger[ListChange, ListTestCase](ListTestCase.apply, noSetBangs)

  val noSetBangs: ListTestCase => Boolean = (c: ListTestCase) => {
    c.changes.find(c => c.s == "set!") match {
      case Some(_) => false
      case None => true
    }
  }

  def main(args: Array[String]): Unit = {
    println()
    println("")
    println("simplifying case: " + testCaseFail)
    println("simplified case: " + ListDeltaDebugger.ddmin(testCaseFail))
  }
}
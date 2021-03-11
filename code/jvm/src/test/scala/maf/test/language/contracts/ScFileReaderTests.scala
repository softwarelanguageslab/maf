package maf.test.language.contracts

import maf.language.contracts.ScProgram
import maf.test.ScTestsJVMLocalStore
import maf.test.modular.contracts.FileTests

class ScFileReaderTests extends FileTests with ScTestsJVMLocalStore {
  "safe_unsafe_annotations.rkt" should "be parsed correctly" in {
    val source   = fromFile("./test/soft-contract/safe_unsafe_annotations.rkt")
    val compiled = compile(source)
    compiled should matchPattern {
      case _: ScProgram =>
    }
  }

  evalFromFile("./test/soft-contract/safe_unsafe_annotations.rkt").full()
}

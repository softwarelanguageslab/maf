package maf.test.deltaDebugging.gtr.transformations
import maf.language.scheme.SchemeExp
import org.scalatest.flatspec.AnyFlatSpec

class AnyFlatSpecTransformations extends AnyFlatSpec {
  def assertTreeString(treeString: String, suggestedTrees: List[SchemeExp]): Unit =
    assert(suggestedTrees.exists(tree => {
      tree.toString equals treeString
    }))
}

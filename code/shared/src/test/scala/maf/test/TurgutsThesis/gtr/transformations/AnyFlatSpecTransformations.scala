package maf.test.TurgutsThesis.gtr.transformations
import maf.language.scheme.SchemeExp
import org.scalatest.flatspec.AnyFlatSpec

class AnyFlatSpecTransformations extends AnyFlatSpec {
  var suggestedTrees: List[SchemeExp] = List()

  def checkSuggestedTreeString(treeString: String): Unit =
    assert(suggestedTrees.exists(tree => {
      tree.toString equals treeString
    }))
}

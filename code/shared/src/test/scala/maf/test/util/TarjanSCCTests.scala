package maf.test.util

import maf.test.UtilityTest
import maf.util.graph.Tarjan
import org.scalatest.propspec.AnyPropSpec

class TarjanSCCTests extends AnyPropSpec {

  case class TarjanGraph[Node](
      id: Int,
      nodes: Set[Node],
      edges: Map[Node, Set[Node]],
      expectedResult: Set[Set[Node]]) {
    def test(): Unit = property(s"Tarjan correctly computes SCCs for graph $id.", UtilityTest) {
      val scc = Tarjan.scc(nodes, edges)
      assert(scc == expectedResult, s"Tarjan SCC returned $scc for graph $id, whilst $expectedResult was expected.")
    }
  }

  private case class Node(name: Int)

  val graphs: List[TarjanGraph[_]] = List(
    TarjanGraph(1, Set(1, 2, 3, 4), Map((2, Set(3)), (3, Set(2, 4))), Set(Set(2, 3))),
    TarjanGraph(
      2,
      Set("a", "b", "c", "d", "e", "f", "g", "h", "i"),
      Map(
        ("a", Set("b", "e")),
        ("b", Set("a", "f")),
        ("c", Set("b", "g", "h")),
        ("d", Set("c")),
        ("e", Set("a", "f")),
        ("f", Set("g")),
        ("g", Set("f")),
        ("h", Set("d", "g")),
        ("i", Set("d", "h", "i"))
      ),
      Set(Set("a", "b", "e"), Set("c", "d", "h"), Set("f", "g"), Set("i"))
    ),
    TarjanGraph(
      3,
      Set(Node(0), Node(1), Node(2), Node(3), Node(4), Node(5), Node(6), Node(7)),
      Map(
        (Node(0), Set(Node(1), Node(4))),
        (Node(1), Set(Node(5))),
        (Node(2), Set(Node(1), Node(3), Node(6))),
        (Node(3), Set(Node(6))),
        (Node(4), Set(Node(0), Node(5))),
        (Node(5), Set(Node(2), Node(6))),
        (Node(6), Set(Node(7))),
        (Node(7), Set(Node(3)))
      ),
      Set(Set(Node(0), Node(4)), Set(Node(1), Node(2), Node(5)), Set(Node(3), Node(6), Node(7)))
    ),
    TarjanGraph(4,
                Set(1, 2, 3, 4, 5, 6, 7, 8, 9),
                Map((1, Set(2)), (2, Set(3)), (3, Set(1)), (4, Set(6)), (5, Set(5)), (7, Set(8)), (8, Set(7))),
                Set(Set(1, 2, 3), Set(5), Set(7, 8))
    )
  )

  graphs.foreach(_.test())
}

package maf.test.util

import maf.test.UtilityTest
import maf.util.graph.SCC
import org.scalatest.propspec.AnyPropSpec

class IncrementalSCCTests  extends AnyPropSpec:

    case class TarjanGraph[N](id: Int,
                              nodes: Set[N],
                              edges: Map[N, Set[N]], // Including added edges, excluding removed edges.
                              addedEdges: Map[N, Set[N]],
                              removedEdges: Map[N, Set[N]],
                              previousSCCs: Set[Set[N]],
                              expectedResult: Set[Set[N]]):
        def test(): Unit = property(s"Tarjan correctly computes SCCs for graph $id.", UtilityTest) {
            val scc = SCC.incremental(nodes, edges, addedEdges, removedEdges, previousSCCs)
            assert(scc == expectedResult, s"Incremental SCC returned $scc for graph $id, whilst $expectedResult was expected.")
        }

    private case class Node(name: Int)
    private sealed trait N
    private case class A(name: Int) extends N
    private case class L(name: Int) extends N
    private case class C(name: Int) extends N

    val graphs: List[TarjanGraph[_]] = List(
        TarjanGraph(1, Set(1, 2, 3, 4), Map((2, Set(3)), (3, Set(2, 4))),
            Map((4, Set(1, 2))), Map(), Set(Set(2, 3)), Set(Set(2, 3, 4))),
        TarjanGraph(
            2,
            Set("a", "b", "c", "d", "e", "f", "g", "h", "i"),
            Map(
                ("a", Set("b", "e")),
                ("b", Set("a", "f")),
                ("c", Set("b", "g", "h")),
                ("d", Set("c")),
                ("e", Set("f")),
                ("f", Set("g")),
                ("g", Set("d", "f")),
                ("h", Set("g")),
                ("i", Set("d", "h"))
            ),
            Map(("g", Set("d"))),
            Map(("i", Set("i")), ("e", Set("a")), ("h", Set("d"))),
            Set(Set("a", "b", "e"), Set("c", "d", "h"), Set("f", "g"), Set("i")),
            Set(Set("a", "b", "c", "d", "e", "f", "g", "h"))
        ),
        TarjanGraph(
            3,
            Set(Node(0), Node(1), Node(2), Node(3), Node(4), Node(5), Node(7)),
            Map(
                (Node(0), Set(Node(1), Node(4))),
                (Node(1), Set(Node(5))),
                (Node(2), Set(Node(1), Node(3))),
                (Node(3), Set()),
                (Node(4), Set(Node(0), Node(5))),
                (Node(5), Set(Node(2))),
                (Node(7), Set(Node(3)))
            ),
            Map(),
            Map((Node(2), Set(Node(6))), (Node(3), Set(Node(6))), (Node(5), Set(Node(6))), (Node(6), Set(Node(7)))),
            Set(Set(Node(0), Node(4)), Set(Node(1), Node(2), Node(5)), Set(Node(3), Node(6), Node(7))),
            Set(Set(Node(0), Node(4)), Set(Node(1), Node(2), Node(5)))
        ),
        TarjanGraph(4,
            Set(1, 2, 3, 4, 5, 6, 7, 8, 9),
            Map((1, Set(2)), (2, Set(3)), (3, Set(1)), (4, Set(6)), (5, Set(5)), (7, Set(8)), (8, Set(7))),
            Map((6, Set(7))), Map(),
            Set(Set(1, 2, 3), Set(5), Set(7, 8)), Set(Set(1, 2, 3), Set(5), Set(7, 8))
        ),
        // `sedgewick172` from the AD course.
        TarjanGraph(5,
            Set(0, 1, 2, 3, 4, 5),
            Map((0, Set(2, 5)), (1, Set(0)), (2, Set(1)), (3, Set(2, 4)), (4, Set(5)), (5, Set(4))),
            Map((3, Set(3))), Map((3, Set(2, 4))),
            Set(Set(0, 1, 2), Set(4, 5)), Set(Set(0, 1, 2), Set(4, 5), Set(3))
        ),
        // `sedgewick172-bis` from the AD course.
        TarjanGraph(6,
            Set(0, 1, 2, 3, 4, 5),
            Map((0, Set(1)), (1, Set(2)), (2, Set(0, 3)), (4, Set(0, 3, 5)), (5, Set(0, 4))),
            Map((0, Set(1)), (1, Set(2)), (2, Set(0, 3)), (4, Set(0, 3, 5)), (5, Set(0, 4))),
            Map((0, Set(2, 4, 5)), (1, Set(0)), (2, Set(1)), (3, Set(2, 4)), (4, Set(5)), (5, Set(4))),
            Set(Set(0, 1, 2), Set(4, 5)),
            Set(Set(0, 1, 2), Set(4, 5))
        ),
        // `sedgewick172-tris` from the AD course.
        TarjanGraph(7,
            Set(0, 1, 2, 3, 4, 5),
            Map((0, Set(0, 2, 4, 5)), (1, Set(0)), (2, Set(1)), (3, Set(2, 4)), (4, Set(5)), (5, Set(4))),
            Map(),
            Map((4, Set(2))),
            Set(Set(0, 1, 2, 4, 5)),
            Set(Set(0, 1, 2), Set(4, 5))
        ),
        // `full-cycle` from the AD course.
        TarjanGraph(8,
            Set(0, 1, 2, 3, 4, 5),
            Map((0, Set(1, 2)), (1, Set(2, 3)), (2, Set(0, 5)), (5, Set(4, 5)), (4, Set(2, 3)), (3, Set(0, 1))),
            Map((0, Set(2)), (1, Set(3)), (2, Set(0)), (5, Set(5)), (4, Set(2)), (3, Set(1))),
            Map(),
            Set(Set(0, 1, 2, 3, 4, 5)),
            Set(Set(0, 1, 2, 3, 4, 5))
        ),
        // `a-list` from the AD course.
        TarjanGraph[Int](
            9,
            Set(0, 1, 2, 3, 4, 5),
            Map((0, Set(1)), (1, Set(2)), (2, Set(0, 5)), (5, Set(4)), (4, Set(3)), (3, Set(5))),
            Map((2, Set(0)), (3, Set(5))),
            Map(),
            Set(),
            Set(Set(0, 1, 2), Set(3, 4, 5))
        ),
        // `dag-1` from the AD course.
        TarjanGraph[Int](
            10,
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
            Map(
                (0, Set(1, 2, 3, 5, 6)),
                (2, Set(3)),
                (3, Set(4, 5)),
                (4, Set(9)),
                (6, Set(4, 9)),
                (7, Set(6)),
                (8, Set(7)),
                (9, Set(10, 11, 12)),
                (11, Set(12))
            ),
            Map((9, Set(0))),
            Map(),
            Set(),
            Set(Set(0, 2, 3, 4, 6, 9))
        ),
        // `scc-4` from the AD course.
        TarjanGraph(
            11,
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
            Map(
                (0, Set(1, 5, 6)),
                (2, Set(0, 3)),
                (3, Set(5)),
                (4, Set(3, 11)),
                (5, Set(4)),
                (6, Set(4, 9)),
                (7, Set(6, 8)),
                (8, Set(7, 9)),
                (9, Set(10, 11)),
                (10, Set(5, 12)),
                (11, Set(12)),
                (12, Set(9))
            ),
            Map((10, Set(5))),
            Map((3, Set(2)), (4, Set(2))),
            Set(Set(0, 2, 3, 4, 5, 6), Set(7, 8), Set(9, 10, 11, 12)),
            Set(Set(3, 4, 5, 9, 10, 11, 12), Set(7, 8))
        ),
        TarjanGraph(
            12,
            Set("a", "b", "c", "d", "e", "f", "g", "h", "i"),
            Map(
                ("a", Set("b", "e")),
                ("b", Set("a", "f")),
                ("c", Set("b", "g", "h")),
                ("d", Set("c")),
                ("e", Set("f")),
                ("f", Set("g")),
                ("g", Set("f")), // Copy of 2 but no "d" here.
                ("h", Set("g")),
                ("i", Set("d", "h"))
            ),
            Map(),
            Map(("i", Set("i")), ("e", Set("a")), ("h", Set("d"))),
            Set(Set("a", "b", "e"), Set("c", "d", "h"), Set("f", "g"), Set("i")),
            Set(Set("a", "b"), Set("f", "g"))
        ),
        TarjanGraph[N](
            13,
            ((1 to 5).map(L.apply) ++ (1 to 5).map(C.apply) ++ (1 to 16).map(A.apply)).toSet,
            Map(
                (A(1), Set(C(1))),
                (A(4), Set(C(3))),
                (A(5), Set(A(8))),
                (A(6), Set(C(4))),
                (A(7), Set(A(10))),
                (A(8), Set(A(9))),
                (A(9), Set(A(12))),
                (A(10), Set(A(14))),
                (A(11), Set(C(5))),
                (A(12), Set(A(13), L(5), C(5))),
                (A(14), Set(A(15))),
                (A(15), Set(A(16))),
                (A(16), Set(A(16), A(7))),
                (L(1), Set(A(2))),
                (L(2), Set(A(5))),
                (L(3), Set(A(16))),
                (L(4), Set(A(16))),
                (L(5), Set(A(16))),
                (C(1), Set(C(2), A(5), A(12))),
                (C(2), Set(C(3), A(9))),
                (C(3), Set(A(8))),
                (C(4), Set(A(7), A(16))),
                (C(5), Set(A(15)))
            ),
            Map(
                (A(1), Set(C(1))),
                (A(4), Set(C(3))),
                (A(5), Set(A(8))),
                (A(6), Set(C(4))),
                (A(7), Set(A(10))),
                (A(8), Set(A(9))),
                (A(9), Set(A(12))),
                (A(10), Set(A(14))),
                (A(11), Set(C(5))),
                (A(12), Set(A(13), L(5), C(5))),
                (A(14), Set(A(15))),
                (A(15), Set(A(16))),
                (A(16), Set(A(16), A(7))),
                (L(1), Set(A(2))),
                (L(2), Set(A(5))),
                (L(3), Set(A(16))),
                (L(4), Set(A(16))),
                (L(5), Set(A(16))),
                (C(1), Set(C(2), A(5), A(12))),
                (C(2), Set(C(3), A(9))),
                (C(3), Set(A(8))),
                (C(4), Set(A(7), A(16))),
                (C(5), Set(A(15)))
            ),
            Map(),
            Set(),
            Set(Set(A(7), A(10), A(14), A(15), A(16)))
        ),
        TarjanGraph(
            14,
            (1 to 10).toSet,
            Map(
                (2, Set(3, 5)),
                (3, Set(8)),
                (4, Set(1, 2, 4, 6, 7)),
                (5, Set(10)),
                (7, Set(10)),
                (8, Set(4)),
                (9, Set(10)),
            ),
            Map(
                (4, Set(2, 4, 7)),
                (9, Set(10))
            ),
            Map(),
            Set(),
            Set(Set(2, 3, 4, 8))
        )
    )

    graphs.foreach(_.test())

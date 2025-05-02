package maf.test.util

import org.scalatest.propspec.AnyPropSpec
import maf.test.UtilityTest
import maf.util.graph.TopSort
import maf.util.graph.SCC

class TopSortTests extends AnyPropSpec:
    def testTopSort(id: Int, nodes: Set[String], edges: Map[String, Set[String]], collapse: Boolean = false)(expectedResult: Any): Unit =
        property(s"TopSort correctly topologically sorts graph with id $id", UtilityTest) {
            if collapse then
                val (nodes1, edges1) = SCC.collapse(nodes, edges)
                val topsort = TopSort.topsort(nodes1.toList, edges1)
                assert(topsort == expectedResult)
            else
                val topsort = TopSort.topsort(nodes.toList, edges)
                assert(topsort == expectedResult)
        }

    testTopSort(1,
                Set("0", "1", "2", "3", "4", "5", "6"),
                Map(
                  "0" -> Set("1"),
                  "1" -> Set("2", "3"),
                  "3" -> Set("4"),
                  "5" -> Set("6"),
                  "6" -> Set("3")
                )
    ) {
        List("5", "0", "6", "1", "2", "3", "4"),
    }

    testTopSort(2,
                Set("0", "1", "2", "3", "4", "5"),
                Map(
                  "0" -> Set("1"),
                  "1" -> Set("2"),
                  "2" -> Set("3"),
                  "3" -> Set("4"),
                  "4" -> Set("5", "2")
                ),
                collapse = true
    ) {
        List(Set("0"), Set("1"), Set("2", "3", "4"), Set("5"))
    }

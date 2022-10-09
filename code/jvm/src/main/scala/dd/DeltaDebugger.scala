package dd

abstract class DeltaDebugger[C <: Change, T <: TestCase[C]]
(factory: List[C] => T, test: T => Boolean) {

    val x: Int = 10

    def ddmin(caseToMinimize: T): T = {
        ddmin2(caseToMinimize, 2)
    }

    private def ddmin2(caseToMinimize: T, n: Int): T = { //n indicates the number of subsets
        val subsets: List[T] = makeSubsets(caseToMinimize, n)
        val complements: List[T] = findComplements(caseToMinimize, subsets)

        subsets.find(testCase => !test(testCase)) match {
            case Some(subset) => ddmin2(subset, 2) //reduce to subset
            case None =>
                complements.find(testCase => !test(testCase)) match {
                    case Some(complement) =>
                        ddmin2(complement, Math.max(n - 1, 2)) //reduce to complement
                    case None =>
                        if n < caseToMinimize.changes.length then
                            ddmin2(caseToMinimize, Math.min(caseToMinimize.changes.length, 2 * n))
                        else caseToMinimize
                }
        }
    }

    private def makeSubsets(c: T, n: Int): List[T] = {
        val subsetSize: Int = c.changes.length / n
        var i: Int = 0
        var res: List[T] = List()

        while c.changes.slice(i, i + subsetSize).nonEmpty do //while there is still a slice to collect
            val x: T = factory(c.changes.slice(i, i + subsetSize))
            res = res.::(x) //collect it
            i = i + subsetSize //repeat
        res
    }

    private def findComplements(c: T, subsets: List[T]): List[T] = {
        subsets.map(subset => {
            factory(c.changes.filterNot(c => subset.changes.contains(c)))
        })
    }
}

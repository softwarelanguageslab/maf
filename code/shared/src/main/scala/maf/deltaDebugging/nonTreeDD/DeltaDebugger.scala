package maf.deltaDebugging.nonTreeDD

import maf.deltaDebugging.nonTreeDD.AST.Instruction

object DeltaDebugger:
  type Program = List[Instruction]

  def ddmin(program: Program, oracle: Program => Boolean): Program =
      ddmin2(program, oracle, 2)

  private def ddmin2(program: Program, oracle: Program => Boolean, n: Int): Program = //n indicates the number of subsets
    val subsets: List[Program] = makeSubsets(program, n)
    val complements: List[Program] = findComplements(program, subsets)

    subsets.find(testCase => oracle(testCase)) match
      case Some(subset) => ddmin2(subset, oracle, 2) //reduce to subset
      case None =>
        complements.find(testCase => oracle(testCase)) match
          case Some(complement) =>
            ddmin2(complement, oracle, Math.max(n - 1, 2)) //reduce to complement
          case None =>
            if n < program.length then
              ddmin2(program, oracle, Math.min(program.length, 2 * n))
            else program

  private def makeSubsets(program: List[Instruction], n: Int): List[List[Instruction]] =
    val subsetSize: Int = program.length / n
    var i: Int = 0
    var res: List[List[Instruction]] = List()

    while program.slice(i, i + subsetSize).nonEmpty do //while there is still a slice to collect
      val x: List[Instruction] = program.slice(i, i + subsetSize)
      res = res.::(x) //collect it
      i = i + subsetSize //repeat
    res

  private def findComplements(program: List[Instruction], subsets: List[List[Instruction]]): List[List[Instruction]] =
    subsets.map(subset => {
      program.filterNot(c => subset.contains(c))
    })

package maf.deltaDebugging.nonTreeDD

import maf.deltaDebugging.nonTreeDD.AST.*
import maf.deltaDebugging.nonTreeDD.Interpreter.*

object Main {

  val program: List[Instruction] =
    List(
      SetConstant(Reg1, 10),
      SetConstant(Reg2, 0),

      Set(Reg3, Mul(Reg1, Reg2)),
      Set(Reg4, Plus(Reg1, Reg2)),
      Set(Reg5, Mul(Reg1, Reg2)),
      Set(Reg6, Sub(Reg1, Reg2)),

      SetConstant(RegReturn, 200) //ok
    )

  def main(args: Array[String]): Unit = {
    val result = Interpreter.eval(program)
    println(result)
  }
}
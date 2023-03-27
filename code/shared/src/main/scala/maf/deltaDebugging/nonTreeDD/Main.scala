package maf.deltaDebugging.nonTreeDD

import maf.deltaDebugging.nonTreeDD.AST.*
import maf.deltaDebugging.nonTreeDD.Interpreter.*

object Main {

  type Program = List[Instruction]
  val program: List[Instruction] =
    List(
      SetConstant(Reg1, 10),
      SetConstant(Reg2, 0),

      Set(Reg3, Mul(Reg1, Reg2)),
      Set(Reg4, Plus(Reg1, Reg2)),
      Set(Reg5, Div(Reg1, Reg2)),
      Set(Reg6, Sub(Reg1, Reg2)),

      SetConstant(RegReturn, 200) //ok
    )

  def main(args: Array[String]): Unit = {
    val oracle: Program => Boolean = p => {
      try
        Interpreter.eval(p)
        false
      catch
        case _: Throwable => true
    }

    val reduced = DeltaDebugger.ddmin(program, oracle)
    println(reduced)
  }
}
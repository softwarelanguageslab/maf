package maf.deltaDebugging.nonTreeDD

import maf.deltaDebugging.nonTreeDD.AST.Instruction
import maf.deltaDebugging.nonTreeDD.AST.*

object Interpreter:
  type Program = List[Instruction]
  abstract class Register(var value: Int)
  case object Reg1 extends Register(-1)
  case object Reg2 extends Register(-1)
  case object Reg3 extends Register(-1)
  case object Reg4 extends Register(-1)
  case object Reg5 extends Register(-1)
  case object Reg6 extends Register(-1)
  case object RegReturn extends Register(-1)

  def eval(program: Program): Int =
    Reg1.value = -1
    Reg2.value = -1
    Reg3.value = -1
    Reg4.value = -1
    Reg5.value = -1
    Reg6.value = -1
    RegReturn.value = -1
    evalProgram(program)

  private def evalProgram(program: Program): Int =
    program match
      case instr :: rest =>
        instr match
          case Set(destination, binOp) =>
            val v1 = binOp.l.value
            val v2 = binOp.r.value
            binOp match
              case _: Plus =>
                destination.value = v1 + v2
              case _: Mul =>
                destination.value = v1 * v2
              case _: Div =>
                destination.value = v1 / v2
              case _: Sub =>
                destination.value = v1 - v2
          case SetConstant(destination, const) =>
            destination.value = const
        evalProgram(rest)

      case Nil =>
        RegReturn.value


package maf.deltaDebugging.nonTreeDD

import maf.deltaDebugging.nonTreeDD.AST.Instruction
import maf.deltaDebugging.nonTreeDD.AST.*

object Interpreter:
  type Program = List[Instruction]
  case class Register(var value: Int)
  object Reg1 extends Register(-1)
  object Reg2 extends Register(-1)
  object Reg3 extends Register(-1)
  object Reg4 extends Register(-1)
  object Reg5 extends Register(-1)
  object Reg6 extends Register(-1)
  object RegReturn extends Register(-1)



  def eval(program: Program): Int =
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
                destination.value = v1 / v2
          case SetConstant(destination, const) =>
            destination.value = const
        eval(rest)

      case Nil =>
        RegReturn.value


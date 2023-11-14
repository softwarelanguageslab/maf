package maf.deltaDebugging.nonTreeDD

import maf.deltaDebugging.nonTreeDD.Interpreter.Register

object AST:
  abstract class Instruction
  case class SetConstant(destination: Register, const: Int) extends Instruction
  case class Set(destination: Register, binop: BinOp) extends Instruction

  abstract class BinOp(val l: Register, val r: Register)
  case class Plus(override val l: Register, override val r: Register) extends BinOp(l, r)
  case class Sub(override val l: Register, override val r: Register) extends BinOp(l, r)
  case class Mul(override val l: Register, override val r: Register) extends BinOp(l, r)
  case class Div(override val l: Register, override val r: Register) extends BinOp(l, r)



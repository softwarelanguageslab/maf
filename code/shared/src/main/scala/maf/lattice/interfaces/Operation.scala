package maf.lattice.interfaces

/** An operation represents a primitive operation as available in many programming languages. */
trait Operation {

  /** The arity of the operation */
  val arity: Int

  /* The name of the operation */
  val name: String

  /** The error message that gets generated when the arity of the operation does not match the argument list */
  def arityException[L](args: List[L]): String =
    s"Operation ${name} expects ${arity} arguments but got ${args.size}"

  /* Checks the argument list conforms to the arity of the operation, if not throws an exception */
  def checkArity[L](args: List[L]): Unit =
    if (args.size != arity) {
      // This is a runtime error because a lattice operation is improperly called by the analysis developper
      throw new Exception(arityException(args))
    }
}

abstract class Operation1(val name: String) extends Operation {
  val arity = 1
}

abstract class Operation2(val name: String) extends Operation {
  val arity = 2
}

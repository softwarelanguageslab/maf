package maf.language.ContractScheme.interpreter
import maf.language.ContractScheme.ContractValues

object ConcreteValues:
    export maf.language.scheme.interpreter.ConcreteValues.*

    case class ContractValue(value: ContractValues.Value[Value]) extends Value

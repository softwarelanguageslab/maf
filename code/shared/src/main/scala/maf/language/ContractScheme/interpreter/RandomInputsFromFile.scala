package maf.language.ContractScheme.interpreter

class RandomInputsFromFile(sourcePath: String) extends RandomInputGenerator:
    override def generateInput(contract: Set[String], topLevelFunction: Option[String] = None): ConcreteValues.Value = ???

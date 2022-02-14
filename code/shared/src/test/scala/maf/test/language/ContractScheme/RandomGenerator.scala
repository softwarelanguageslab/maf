package maf.test.language.ContractScheme

import maf.test.language.scheme.*
import maf.language.ContractScheme.interpreter.*

/** A generator that generates random input regardless of the contract and topLevelFunction parameter */
class PurelyRandomGenerator(primitives: List[String]) extends RandomInputGenerator:
    private val generator: ConcreteGenerators = ConcreteGenerators(primitives)
    override def generateInput(topLevelFunction: String, contract: Set[String] = Set()): List[ConcreteValues.Value] =
      List(generator.any.sample.get)

package maf.test.deltaDebugging.soundnessDD.evaluation

import maf.language.scheme._

object CountLambdaBindings:
  private var lambdaBindings = 0

  def count(program: SchemeExp): Int =
    lambdaBindings = 0
    program.forEach(subExp => {
      subExp match
        case exp: SchemeLettishExp =>
          exp.bindings.collect({
            case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
          }).foreach(b => lambdaBindings += 1)

        case SchemeDefineVariable(name, value, idn) =>
          value match
            case _: SchemeLambdaExp => lambdaBindings += 1
            case _ =>
        case _ =>
    })
    lambdaBindings
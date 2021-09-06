package maf.language.ContractScheme

import maf.language.scheme.BaseSchemeCompiler
import maf.language.sexp.SExp
import maf.language.scheme._
import maf.core.{Identifier, Identity}

object ContractSchemeCompiler extends BaseSchemeCompiler:
  import scala.util.control.TailCalls._
  import maf.language.sexp.SExpUtils._
  import maf.util.TailrecUtil._

  private def compile_sequence(seq: SExp): TailRec[List[SchemeExp]] =
    sequence(smap(seq, this._compile))

  // TODO: create special kind of Lambda expression that ignores its arguments
  private def rangeToRangerMaker(range: SchemeExp): SchemeExp =
    SchemeLambda(None, List(Identifier("0arg", Identity.none)), List(range), range.idn)

  // TODO: also take vararg into account eg. (define (f . a) exp) is a function that takes any number of arguments
  private def compile_params(params: SExp): List[Identifier] = params match
    case IdentWithIdentity(name, idn) :::: rest =>
      Identifier(name, idn) :: compile_params(rest)
    case snil => List()

  override def _compile(exp: SExp): TailRec[SchemeExp] = exp match
    // (-> contract1 contract2 range)
    case Ident("->" | "~>") :::: contracts =>
      for
        compiledContracts <- compile_sequence(contracts)
      yield ContractSchemeDepContract(
        compiledContracts.init,
        rangeToRangerMaker(compiledContracts.last),
        exp.idn
      )

    // (~ contract1 contract2 rangeMaker)
    case Ident("~" | "->d") :::: contracts =>
      for
        compiledContracts <- compile_sequence(contracts)
      yield ContractSchemeDepContract(
        compiledContracts.init,
        compiledContracts.last,
        exp.idn
      )

    // (flat expr)
    case Ident("flat") :::: expr :::: snil =>
      for
        compiledExpr <- tailcall(_compile(expr))
      yield ContractSchemeFlatContract(compiledExpr, expr.idn)

    case Ident("flat") :::: _ => throw new Exception(s"Parse error, flat expects exactly one argument at ${exp.idn}")

    // (mon contract expr)
    case Ident("mon") :::: contract :::: expr :::: snil =>
      for
        compiledContract <- tailcall(_compile(contract))
        compiledExpr <- tailcall(_compile(expr))
      yield ContractSchemeMon(compiledContract, compiledExpr, expr.idn)

    case Ident("mon") :::: _ => throw new Exception(s"Parse error, mon expects exactly two arguments at ${exp.idn}")

    // (define/contract (f argument ...) contract expr ...)
    case Ident("define/contract") :::: (IdentWithIdentity(f, idn) :::: params) :::: contract :::: expressions =>
      for
        compiledParams <- done(compile_params(params))
        compiledContract <- tailcall(_compile(contract))
        compiledExpressions <- compile_sequence(expressions)
      yield ContractSchemeDefineContract(
        Identifier(f, idn),
        compiledParams,
        compiledContract,
        SchemeBegin(
          compiledExpressions,
          Identity.none
        ),
        exp.idn
      )

    case Ident("define/contract") :::: _ => throw new Exception(s"Parse error, invalid usage of define/contract at ${exp.idn}")

    case _ => super._compile(exp)

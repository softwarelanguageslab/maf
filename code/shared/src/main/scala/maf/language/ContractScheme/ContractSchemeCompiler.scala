package maf.language.ContractScheme

import maf.language.scheme.BaseSchemeCompiler
import maf.language.sexp.SExp
import maf.language.scheme._
import maf.core.{Identity, Identifier}

object ContractSchemeCompiler extends BaseSchemeCompiler {
  import scala.util.control.TailCalls._ 
  import maf.language.sexp.SExpUtils._
  import maf.util.TailrecUtil._

  private def compile_contracts(contracts: SExp): TailRec[List[SchemeExp]] =
     sequence(smap(contracts, this._compile))

  // TODO: create special kind of Lambda expression that ignores its arguments
  private def rangeToRangerMaker(range: SchemeExp): SchemeExp = 
    SchemeLambda(None, List(Identifier("0", Identity.none)), List(range), range.idn)

  override def _compile(exp: SExp): TailRec[SchemeExp] = exp match {
    // (-> contract1 contract2 range)
    case Ident("->" | "~>") :::: contracts => for {
      compiledContracts <- compile_contracts(contracts)
    } yield ContractSchemeDepContract(
      compiledContracts.init, 
      rangeToRangerMaker(compiledContracts.last), 
      exp.idn
    )
    
    // (~ contract1 contract2 rangeMaker)
    case Ident("~" | "->d")  :::: contracts => for {
      compiledContracts <- compile_contracts(contracts)
    } yield ContractSchemeDepContract(
      compiledContracts.init, 
      compiledContracts.last, 
      exp.idn
    )

    // (flat expr)
    case Ident("flat") :::: expr :::: snil => for {
      compiledExpr <- tailcall(_compile(expr))
    } yield ContractSchemeFlatContract(compiledExpr, expr.idn)

    // (mon contract expr)
    case Ident("mon") :::: contract :::: expr => for {
      compiledContract <- tailcall(_compile(contract))
      compiledExpr <- tailcall(_compile(expr))
    } yield ContractSchemeMon(compiledContract, compiledExpr, expr.idn)

    case _ => super._compile(exp)
  }
}


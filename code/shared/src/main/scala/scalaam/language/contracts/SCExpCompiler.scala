package scalaam.language.contracts

import scalaam.core.{Identifier, Identity}
import scalaam.language.sexp.{SExp, SExpId, SExpPair, SExpValue, ValueNil, ValueString}

/**
  * Compiles a program of S-expressions into a program of ScExp
  */
object SCExpCompiler {
  object :: {
    def unapply(value: SExp): Option[(SExp, SExp)] = {
      value match {
        case SExpPair(car, cdr, _) => Some((car, cdr))
        case _                     => None
      }
    }
  }

  object Ident {
    def unapply(value: SExpId): Option[(String)] = Some(value.id.name)
  }

  object IdentWithIdentity {
    def unapply(arg: SExpId): Option[(String, Identity)] =
      Some((arg.id.name, arg.idn))
  }

  object ListNil {
    def unapply(value: SExp): Option[(Identity)] = value match {
      case SExpValue(ValueNil, idn) => Some((idn))
      case _                        => None
    }
  }

  case class SCExpCompilerException(message: String) extends Exception

  def compile_params(s: SExp): List[ScIdentifier] = s match {
    case SExpPair(IdentWithIdentity(name, idn), cdr, _) =>
      ScIdentifier(name, idn) :: compile_params(cdr)
    case SExpValue(ValueNil, _) => List()
  }

  def compile_sequence(s: SExp): List[ScExp] = s match {
    case SExpPair(exp, cdr, _)  => compile(exp) :: compile_sequence(cdr)
    case SExpValue(ValueNil, _) => List()
  }

  def compile(prog: SExp): ScExp = prog match {
    case Ident("set!") :: IdentWithIdentity(name, idn) :: exp :: SExpValue(ValueNil, _) =>
      ScSet(ScIdentifier(name, idn), compile(exp), prog.idn)

    case Ident("flat") :: expr :: ListNil(_) =>
      ScFlatContract(compile(expr), prog.idn)

    case Ident("~>") :: domain :: range :: ListNil(_) =>
      val domainCompiled = compile(domain)
      val rangeCompiled  = compile(range)
      ScHigherOrderContract(domainCompiled, rangeCompiled, prog.idn)

    case Ident("~") :: domain :: range :: ListNil(_) =>
      val domainCompiled = compile(domain)
      val rangeCompiled  = compile(range)
      ScDependentContract(domainCompiled, rangeCompiled, prog.idn)

    case SExpId(identifier) => ScIdentifier(identifier.name, prog.idn)
    case Ident("lambda") :: params :: expression :: ListNil(_) =>
      val compiledParams     = compile_params(params)
      val compiledExpression = compile(expression)
      ScLambda(compiledParams, compiledExpression, prog.idn)

    case Ident("letrec") :: (IdentWithIdentity(name, idn) :: bindingExpression :: ListNil(_)) :: expression :: ListNil(
          _
        ) =>
      val compiledBindingExpression = compile(bindingExpression)
      val compiledExpression        = compile(expression)
      ScLetRec(ScIdentifier(name, idn), compiledBindingExpression, compiledExpression, prog.idn)

    case Ident("mon") :: contract :: expression :: ListNil(_) =>
      val compiledContract   = compile(contract)
      val compiledExpression = compile(expression)
      ScMon(compiledContract, compiledExpression, prog.idn)

    case Ident("if") :: condition :: consequent :: alternative :: ListNil(_) =>
      val compiledCondition   = compile(condition)
      val compiledConsequent  = compile(consequent)
      val compiledAlternative = compile(alternative)
      ScIf(compiledCondition, compiledConsequent, compiledAlternative, prog.idn)

    case Ident("raise") :: SExpValue(ValueString(str), _) =>
      ScRaise(str, prog.idn)

    case Ident("begin") :: expressions =>
      ScBegin(compile_sequence(expressions), prog.idn)

    case operator :: arguments =>
      println(arguments)
      ScFunctionAp(compile(operator), compile_sequence(arguments), prog.idn)

    case IdentWithIdentity(name, idn) =>
      ScIdentifier(name, idn)

    case SExpValue(value, _) => ScValue(value, prog.idn)
  }
}

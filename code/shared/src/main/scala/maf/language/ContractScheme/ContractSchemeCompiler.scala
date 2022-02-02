package maf.language.ContractScheme

import maf.language.scheme.BaseSchemeCompiler
import maf.language.sexp.SExp
import maf.language.sexp
import maf.language.sexp.Value
import maf.language.scheme._
import maf.util.MonoidImplicits.*
import maf.core.{Identifier, Identity}

object ContractSchemeCompiler extends BaseSchemeCompiler:
    import scala.util.control.TailCalls._
    import maf.language.sexp.SExpUtils._
    import maf.util.TailrecUtil._

    /** Utilities for compiling structs */
    object Struct:
        /** Generate a call (_make_struct_getter tag idx) */
        private def getStructField(tag: String, idx: Int, idn: Identity): SchemeExp =
          MakeStructGetter(tag, idx, idn)

        /** Generate a call (_make_struct_setter tag idx) */
        private def setStructField(tag: String, idx: Int, idn: Identity): SchemeExp =
          MakeStructSetter(tag, idx, idn)

        /** Generate a call (_make_struct_constr nam siz) */
        private def makeStructConstructor(nam: String, size: Int, idn: Identity): SchemeExp =
          MakeStructConstr(nam, size, idn)

        case class CompiledField(name: String, idn: Identity)
        def compileFields(fields: SExp): TailRec[List[CompiledField]] =
          // We will map on the list of fields we got
          done(
            smap(
              fields,
              _ match
                  case (field :::: options)              => throw new Exception("options not supported for structs")
                  case IdentWithIdentity(fieldName, idn) => CompiledField(fieldName, idn)
                  case field                             => throw new Exception(s"Invalid field definition $field")
            )
          )

        def generateConstructor(fields: List[CompiledField], name: String, nameIdn: Identity, structIdn: Identity): TailRec[List[SchemeExp]] =
            // generate a call to _make_struct_constr
            val constructor = makeStructConstructor(name, fields.size, structIdn)
            // Generate a definition
            done(List(SchemeDefineVariable(Identifier(s"make-$name", nameIdn), constructor, structIdn)))

        def generatePredicate(name: String, nameIdn: Identity): TailRec[List[SchemeExp]] =
            val predicate = MakeStructPredicate(name, nameIdn)
            done(List(SchemeDefineVariable(Identifier(s"$name?", nameIdn), predicate, nameIdn)))

        def generateGetters(fields: List[CompiledField], name: String, nameIdn: Identity): TailRec[List[SchemeExp]] =
          done(
            fields
              .foldLeft((0, List[SchemeExp]())) { case ((counter, fields), field) =>
                val getter = getStructField(name, counter, field.idn)
                val defv = SchemeDefineVariable(Identifier(s"$name-${field.name}", field.idn), getter, field.idn)
                (counter + 1, defv :: fields)
              }
              ._2
          )

        def generateSetters(fields: List[CompiledField], name: String, nameIdn: Identity): TailRec[List[SchemeExp]] =
          done(
            fields
              .foldLeft((0, List[SchemeExp]())) { case ((counter, fields), field) =>
                val getter = setStructField(name, counter, field.idn)
                val defv = SchemeDefineVariable(Identifier(s"set-$name-${field.name}!", field.idn), getter, field.idn)
                (counter + 1, defv :: fields)
              }
              ._2
          )

    object MatchCompiler:
        def compilePat(pat: SExp): MatchPat = pat match
            case IdentWithIdentity("_", idn) => WildcardPat(idn)
            case IdentWithIdentity(id, idn) =>
              IdPat(id, idn)
            case Ident("var") :::: IdentWithIdentity(id, idn) :::: SNil(_) =>
              IdPat(id, idn)
            case Ident("quote") :::: anything =>
              QuotePat(anything, pat.idn)
            case Ident("list") :::: patterns =>
              ListPat(smap(patterns, compilePat))
            case Ident("list-rest" | "list*") :::: patterns =>
              val compiledPats = smap(patterns, compilePat)
              assert(compiledPats.size >= 1, "invalid usage of list-rest pattern")
              ListRest(compiledPats.dropRight(1), compiledPats.last)
            case Ident("list-no-order") :::: patterns =>
              ListNoOrderPat(smap(patterns, compilePat), None)
            case Ident("vector") :::: patterns =>
              VectorPat(smap(patterns, compilePat))
            case Ident("or") :::: patterns =>
              OrPat(smap(patterns, compilePat))
            case Ident("and") :::: patterns =>
              AndPat(smap(patterns, compilePat))
            case Ident("not") :::: patterns =>
              NotPat(smap(patterns, compilePat))
            case Ident("?") :::: expr :::: pats =>
              PredPat(compile(expr), smap(pats, compilePat))
            case Ident("app") :::: expr :::: pats =>
              AppPat(compile(expr), smap(pats, compilePat))
            case Ident("struct") :::: Ident(id) :::: (pats @ (SNil(_) | (_ :::: _))) =>
              StructPat(id, smap(pats, compilePat))
            case Ident(id) :::: patterns =>
              StructPat(id, smap(patterns, compilePat))
            case sexp.SExpValue(v, _) => LitPat(v)
            case _                    => throw new Exception(s"unsupported match patern $pat")

        def compileClause(clause: SExp): TailRec[MatchExprClause] = clause match
            // with a "when"
            case pat :::: Ident("#:when") :::: pred :::: body =>
              for
                  compiledBody <- sequence(smap(body, _compile))
                  compiledPat <- done(compilePat(pat))
                  compiledPred <- _compile(pred)
              yield MatchExprClause(compiledPat, compiledBody, Some(compiledPred))
            case pat :::: body =>
              for
                  compiledBody <- sequence(smap(body, _compile))
                  compiledPat <- done(compilePat(pat))
              yield MatchExprClause(compiledPat, compiledBody, None)

            case _ => throw new Exception(s"invalid match clause at ${clause.idn}")

    /** Nest the given expressions using the given function application */
    private def nest(expressions: List[SchemeExp], function: String): SchemeExp = expressions match
        case List(expression) => expression
        case expression :: rest =>
          SchemeFuncall(SchemeVar(Identifier(function, Identity.none)), List(expression, nest(rest, function)), Identity.none)
        case List() => throw new Exception("nest cannot receive an empty list")

    private def compile_sequence(seq: SExp): TailRec[List[SchemeExp]] =
      sequence(smap(seq, this._compile))

    // TODO: create special kind of Lambda expression that ignores its arguments
    private def rangeToRangerMaker(range: SchemeExp): SchemeExp =
      SchemeLambda(None, List(Identifier("0arg", Identity.none)), List(range), None, range.idn)

    // TODO: also take vararg into account eg. (define (f . a) exp) is a function that takes any number of arguments
    private def compile_params(params: SExp): List[Identifier] = params match
        case IdentWithIdentity(name, idn) :::: rest =>
          Identifier(name, idn) :: compile_params(rest)
        case SNil(_) => List()

    private def compileContractOut(binding: SExp): TailRec[ContractSchemeProvideOut] = binding match
        case IdentWithIdentity(name, idn) :::: contract :::: SNil(_) =>
          val compiled_name = Identifier(name, idn)
          for compiled_contract <- _compile(contract)
          yield ContractSchemeContractOut(compiled_name, compiled_contract, binding.idn)

        case _ => throw new Exception(s"invalid syntax for contract-out item at ${binding.idn}")

    private def compile_provides(out: SExp): TailRec[List[ContractSchemeProvideOut]] = out match
        case Ident("contract-out") :::: contractsWithIdentifiers =>
          sequence(smap(contractsWithIdentifiers, compileContractOut))
        /** Export something with another modifier than contract-out */
        case Ident(_) :::: outs => throw new Exception("only support for contract-out in provides")
        /** Export particular identifier */
        case Ident(_) => throw new Exception("only support for contract-out in provides")
        /** All other things are syntax errors */
        case _ => throw new Exception(s"syntax error at ${out.idn}")

    override def _compile(exp: SExp): TailRec[SchemeExp] = exp match
        // (-> contract1 contract2 range)
        case Ident("->" | "~>") :::: contracts =>
          for compiledContracts <- compile_sequence(contracts)
          yield ContractSchemeDepContract(
            compiledContracts.init,
            rangeToRangerMaker(compiledContracts.last),
            exp.idn
          )

        // (~ contract1 contract2 rangeMaker)
        case Ident("~" | "->d") :::: contracts =>
          for compiledContracts <- compile_sequence(contracts)
          yield ContractSchemeDepContract(
            compiledContracts.init,
            compiledContracts.last,
            exp.idn
          )

        // (flat expr)
        case Ident("flat") :::: expr :::: SNil(_) =>
          for compiledExpr <- tailcall(_compile(expr))
          yield ContractSchemeFlatContract(compiledExpr, exp.idn)

        case Ident("flat") :::: _ => throw new Exception(s"Parse error, flat expects exactly one argument at ${exp.idn}")

        // (mon contract expr)
        case Ident("mon") :::: contract :::: expr :::: SNil(_) =>
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
          yield SchemeDefineVariable(
            Identifier(f, idn),
            ContractSchemeMon(compiledContract, SchemeLambda(Some(f), compiledParams, compiledExpressions, None, exp.idn), exp.idn),
            exp.idn
          )

        // functions to the outside world, using the `provide` expression.
        case Ident("provide") :::: outs =>
          for compiled_outs <- sequence(smap(outs, compile_provides)).map(_.flatten)
          yield ContractSchemeProvide(compiled_outs, exp.idn)

        // (check contract valueExpression)
        case Ident("check") :::: contract :::: expression :::: SNil(_) =>
          for
              compiledContract <- tailcall(_compile(contract))
              compiledExpr <- tailcall(_compile(expression))
          yield ContractSchemeCheck(compiledContract, compiledExpr, exp.idn)

        case Ident("define/contract") :::: _ => throw new Exception(s"Parse error, invalid usage of define/contract at ${exp.idn}")

        // (struct id (field ...) properties ...)
        case Ident("struct") :::: IdentWithIdentity(name, nameIdn) :::: (fields @ ((_ :::: _) | SNil(_))) :::: SNil(_) =>
          for
              compiledFields <- Struct.compileFields(fields)
              constructor <- Struct.generateConstructor(compiledFields, name, nameIdn, exp.idn)
              predicate <- Struct.generatePredicate(name, nameIdn)
              getters <- Struct.generateGetters(compiledFields, name, nameIdn)
              setters <- Struct.generateSetters(compiledFields, name, nameIdn)
          yield SchemeBegin(constructor ++ predicate ++ getters ++ setters, exp.idn)

        // desugaring of (struct/c name field-contract ...) into a contract as follows:
        // (and/c (flat name?) (flat (lambda (v) (check contract (_struct_ref v 0)))) ...)
        case Ident("struct/c") :::: IdentWithIdentity(name, nameIdn) :::: fields =>
          val tagCheck = ContractSchemeFlatContract(SchemeVar(Identifier(s"${name}?", nameIdn)), Identity.none)
          for
              fieldsCheck <- sequence(
                smap(
                  fields,
                  field => {
                    val structRef = SchemeFuncall(
                      SchemeVar(Identifier("__struct_ref", Identity.none)),
                      List(SchemeVar(Identifier("v", Identity.none)), SchemeValue(sexp.Value.Integer(0), Identity.none)),
                      Identity.none
                    )
                    for {
                      fieldExpr <- _compile(field)
                      checkExpression = ContractSchemeCheck(fieldExpr, structRef, Identity.none)
                      fieldCheck = ContractSchemeFlatContract(
                        SchemeLambda(None, List(Identifier("v", Identity.none)), List(checkExpression), None, Identity.none),
                        field.idn
                      )
                    } yield fieldCheck
                  }
                )
              )
          yield SchemeFuncall(SchemeVar(Identifier("and/c", exp.idn)), tagCheck :: fieldsCheck, exp.idn)

        // Desugaring of and/c (and/c contract1 contract2 ...) to (and/c contract1 (and/c contract2 ...))
        case Ident("and/c") :::: expressions =>
          for {
            compiledExpressions <- sequence(smap(expressions, _compile))
          } yield nest(compiledExpressions, "and/c")

        // (structid super-id (field ...) properties ...) // unsupported
        case Ident("struct") :::: Ident(_) :::: Ident(_) :::: _ =>
          throw new Exception("structs with super-ids are currently unsupported")

        // (match v [clause body ...]) or (match [clause #:when pred body ...])
        case Ident("match") :::: v :::: clauses =>
          for
              compiledValue <- _compile(v)
              compiledClauses <- sequence(smap(clauses, MatchCompiler.compileClause))
          yield MatchExpr(compiledValue, compiledClauses, exp.idn)

        case Ident("match-lambda") :::: clauses =>
          for compiledClauses <- sequence(smap(clauses, MatchCompiler.compileClause))
          yield SchemeLambda(None,
                             List(Identifier("_x", exp.idn)),
                             List(MatchExpr(SchemeVar(Identifier("_x", exp.idn)), compiledClauses, exp.idn)),
                             None,
                             exp.idn
          )

        case _ => super._compile(exp)

package maf.language.ContractScheme

import maf.language.scheme.BaseSchemeCompiler
import maf.language.sexp.SExp
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
        private def getStructField(tag: String, idx: Int, idn: Identity): SchemeFuncall =
            val tagSym = SchemeValue(Value.Symbol(tag), idn)
            val numVal = SchemeValue(Value.Integer(idx), idn)
            SchemeFuncall(SchemeVar(Identifier("_make_struct_getter", idn)), List(tagSym, numVal), idn)

        /** Generate a call (_make_struct_setter tag idx) */
        private def setStructField(tag: String, idx: Int, idn: Identity): SchemeFuncall =
            val tagSym = SchemeValue(Value.Symbol(tag), idn)
            val numVal = SchemeValue(Value.Integer(idx), idn)
            SchemeFuncall(SchemeVar(Identifier("_make_struct_getter", idn)), List(tagSym, numVal), idn)

        /** Generate a call (_make_struct_constr nam siz) */
        private def makeStructConstructor(nam: String, size: Int, idn: Identity): SchemeFuncall =
            val tagSym = SchemeValue(Value.Symbol(nam), idn)
            val numVal = SchemeValue(Value.Integer(size), idn)
            SchemeFuncall(SchemeVar(Identifier("_make_struct_constr", idn)), List(tagSym, numVal), idn)

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
            val predicateBody: List[SchemeExp] = ???
            done(List(SchemeDefineFunction(Identifier(s"$name?", nameIdn), List(Identifier("x", nameIdn)), predicateBody, nameIdn)))

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
                val defv = SchemeDefineVariable(Identifier(s"$name-${field.name}", field.idn), getter, field.idn)
                (counter + 1, defv :: fields)
              }
              ._2
          )

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

    private def compileContractOut(binding: SExp): TailRec[ContractSchemeProvideOut] = binding match
        case IdentWithIdentity(name, idn) :::: contract :::: snil =>
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
        case Ident("flat") :::: expr :::: snil =>
          for compiledExpr <- tailcall(_compile(expr))
          yield ContractSchemeFlatContract(compiledExpr, exp.idn)

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
          yield SchemeDefineVariable(
            Identifier(f, idn),
            ContractSchemeMon(compiledContract, SchemeLambda(Some(f), compiledParams, compiledExpressions, exp.idn), exp.idn),
            exp.idn
          )

        // In Racket, files are modules and can provide certain
        // functions to the outside world, using the `provide` expression.
        case Ident("provide") :::: outs =>
          for compiled_outs <- sequence(smap(outs, compile_provides)).map(_.flatten)
          yield ContractSchemeProvide(compiled_outs, exp.idn)

        // (check contract valueExpression)
        case Ident("check") :::: contract :::: expression :::: snil =>
          for
              compiledContract <- tailcall(_compile(contract))
              compiledExpr <- tailcall(_compile(expression))
          yield ContractSchemeCheck(compiledContract, compiledExpr, exp.idn)

        case Ident("define/contract") :::: _ => throw new Exception(s"Parse error, invalid usage of define/contract at ${exp.idn}")

        // (struct id (field ...) properties ...)
        case Ident("struct") :::: IdentWithIdentity(name, nameIdn) :::: (fields @ (f :::: fs)) :::: snil =>
          for
              compiledFields <- Struct.compileFields(fields)
              constructor <- Struct.generateConstructor(compiledFields, name, nameIdn, exp.idn)
              predicate <- Struct.generatePredicate(name, nameIdn)
              getters <- Struct.generateGetters(compiledFields, name, nameIdn)
              setters <- Struct.generateSetters(compiledFields, name, nameIdn)
          yield SchemeBegin(constructor ++ predicate ++ getters ++ setters, exp.idn)

        // (struct id super-id (field ...) properties ...) // unsupported
        case Ident("struct") :::: Ident(_) :::: Ident(_) :::: _ =>
          throw new Exception("structs with super-ids are currently unsupported")

        case _ => super._compile(exp)

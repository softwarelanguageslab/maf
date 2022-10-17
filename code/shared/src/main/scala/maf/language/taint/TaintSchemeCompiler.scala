package maf.language.taint

import maf.core.Identifier
import maf.language.scheme.*
import maf.language.sexp.*

import scala.util.control.TailCalls

object TaintSchemeCompiler extends BaseSchemeCompiler:
    import scala.util.control.TailCalls.*

    override def reserved: List[String] = List("source", "sink", "sanitize") ::: super.reserved

    override def _compile(exp: SExp): TailCalls.TailRec[SchemeExp] = exp match
        case SExpPair(SExpId(Identifier("source", _)), SExpPair(SExpId(v), SExpValue(Value.Nil, _), _), _) =>
            if reserved.contains(v.name) then
                throw new SchemeCompilerException(s"Invalid Scheme identifier (reserved): $exp (${exp.idn.pos})", exp.idn)
            done(SchemeSource(v, exp.idn))
        case SExpPair(SExpId(Identifier("source", _)), _, _) =>
            throw new Exception(s"Invalid TaintScheme source: $exp (${exp.idn}).")
        case SExpPair(SExpId(Identifier("sink", _)), SExpPair(SExpId(v), SExpValue(Value.Nil, _), _), _) =>
            if reserved.contains(v.name) then
                throw new SchemeCompilerException(s"Invalid Scheme identifier (reserved): $exp (${exp.idn.pos})", exp.idn)
            done(SchemeSink(v, exp.idn))
        case SExpPair(SExpId(Identifier("sink", _)), _, _) =>
            throw new Exception(s"Invalid TaintScheme sink: $exp (${exp.idn}).")
        case SExpPair(SExpId(Identifier("sanitize", _)), SExpPair(SExpId(v), SExpValue(Value.Nil, _), _), _) =>
            if reserved.contains(v.name) then
                throw new SchemeCompilerException(s"Invalid Scheme identifier (reserved): $exp (${exp.idn.pos})", exp.idn)
            done(SchemeSanitizer(v, exp.idn))
        case SExpPair(SExpId(Identifier("sanitize", _)), _, _) =>
            throw new Exception(s"Invalid TaintScheme sanitize: $exp (${exp.idn}).")
        case _ => super._compile(exp)

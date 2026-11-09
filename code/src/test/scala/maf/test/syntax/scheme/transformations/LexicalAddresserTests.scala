package maf.test.syntax.scheme.transformations

import maf.syntax.Source
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.SchemeLexicalAddresser
import maf.syntax.sexp.SExpParser

import org.scalatest.propspec.AnyPropSpec

class LexicalAddresserTests extends AnyPropSpec:
    def translate(program: String): List[SchemeExp[Surface | Lexical]] =
        SchemeLexicalAddresser.translateProgram(SExpParser.parse(program, Source.Text).map(SchemeParser.compile))

    property("A variable that a let* binds more than once refers to its latest binding") {
        translate("(let* ((x 1) (x (+ x 1))) x)") match
            case List(SchemeLetStar(List((x1, _), (x2, SchemeFuncall(_, List(SchemeVarLex(_, ref1), _), _))), List(SchemeVarLex(_, ref2)), _)) =>
                assert(ref1 == LexicalRef.VarRef(x1))
                assert(ref2 == LexicalRef.VarRef(x2))
            case translated => fail(s"Unexpected translation: $translated")
    }

package maf.test.syntax.scheme.transformations

import maf.semantics.scheme.interpreter.*
import maf.syntax.{Identifier, Position, Source}
import maf.syntax.scheme.*
import maf.syntax.scheme.transformations.SchemeMutableVarBoxer
import maf.util.Timeout

import org.scalatest.propspec.AnyPropSpec

class RenamerTests extends AnyPropSpec:
    // undefines and boxes the given program, as the renamer requires
    def prepare(program: String): SchemeExp[Named] =
        val parsed = SchemeParser.parse(program, Source.Text)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        SchemeParser.undefine(SchemeMutableVarBoxer.transform(prelud))

    // the identifiers of all variables that are bound in the given expression
    def binders(exp: SchemeExp[Any]): List[Identifier] =
        val own = exp match
            case lam: SchemeLambdaExp[Any]  => lam.args ++ lam.varArgId
            case let: SchemeLettishExp[Any] => let.bindings.map(_._1)
            case _                          => Nil
        own ++ exp.subexpressions.flatMap {
            case sub: SchemeExp[Any] => binders(sub)
            case _                   => Nil
        }

    def run(program: SchemeExp[Mutation | Named]): ConcreteValues.Value =
        val interpreter = new SchemeInterpreter((_: Option[Position], _: ConcreteValues.Value) => (), io = new EmptyIO())
        interpreter.run(program, Timeout.none)

    val programs: List[(String, BigInt)] = List(
      // nested variables with the same name
      "(define (f x) (let ((x (+ x 1))) (let ((x (* x 2))) x))) (f 1)" -> 4,
      // a mutable parameter (which gets boxed) with the same name as a global variable
      "(define (counter n) (lambda () (set! n (+ n 1)) n)) (define c (counter 0)) (define n 10) (c) (+ (c) n)" -> 12,
      // names that end in a digit, so the 11th variable named `a` and the first named `a1` must not get the same name
      s"(define (f a1) ${(0 to 10).foldRight("a1")((i, body) => s"(let ((a $i)) $body)")}) (f 42)" -> 42,
      // a let* that binds the same name more than once, of which only the last binding is mutable (and gets boxed)
      "(define (f x) (let* ((x (+ x 1)) (x (* x 2))) (set! x (+ x 1)) x)) (f 1)" -> 5
    )

    programs.foreach { (program, result) =>
        property(s"Renaming gives unique names to the variables of $program, without changing its result") {
            val prepared = prepare(program)
            val renamed: SchemeExp[UniqueNames] = SchemeParser.rename(prepared)
            val before = binders(prepared).map(_.name)
            val after = binders(renamed).map(_.name)
            assert(before.distinct.size < before.size, "The program should bind some name more than once")
            assert(after.distinct.size == after.size, s"Some names are bound more than once after renaming: $after")
            assert(run(prepared) == ConcreteValues.Value.Integer(result))
            assert(run(renamed) == ConcreteValues.Value.Integer(result))
        }
    }

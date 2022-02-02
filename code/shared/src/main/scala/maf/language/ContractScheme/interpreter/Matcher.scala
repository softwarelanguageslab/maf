package maf.language.ContractScheme.interpreter

import maf.language.sexp
import maf.language.scheme.*
import maf.core.*
import maf.lattice.Concrete
import maf.language.ContractScheme.*

/**
 * Constructs a matcher for the given value .
 *
 * @param vlu
 *   the value to match against
 * @param eval
 *   an evaluation function to possibly evaluate unevaluate expressions in the pattern
 */
class Matcher(vlu: ConcreteValues.Value, eval: SchemeExp => ConcreteValues.Value):
    private var bindings: Map[Identifier, ConcreteValues.Value] = Map()

    private def matchesLiteral(pat: sexp.Value, vlu: ConcreteValues.Value): Boolean = (pat, vlu) match
        case (sexp.Value.Boolean(b), ConcreteValues.Value.Bool(b1))        => b == b1
        case (sexp.Value.Symbol(s), ConcreteValues.Value.Symbol(s1))       => s == s1
        case (sexp.Value.Integer(i), ConcreteValues.Value.Integer(i1))     => i == i1
        case (sexp.Value.Real(r), ConcreteValues.Value.Real(r1))           => r == r1
        case (sexp.Value.String(s), ConcreteValues.Value.Str(s1))          => s1 == s1
        case (sexp.Value.Character(c), ConcreteValues.Value.Character(c1)) => c == c1
        case (_, _)                                                        => false

    /**
     * Check whether an identifier is already bound with the given name
     *
     * @param name
     *   the name of the identifier to check
     * @throws an
     *   exception if the identifier is already bound
     */
    private def checkDuplicate(name: String): Unit =
      if bindings.values.toSet.contains(name) then throw new Exception(s"cannot define identifier $name twice in the same pattern")

    /** Reset the internal state of the matcher such that the value can be matched again against a different pattern */
    def reset(): Unit =
      bindings = Map()

    /** Resolve the bindings in the last pattern we matched against */
    def resolveBindings: List[(Identifier, ConcreteValues.Value)] = bindings.toList

    def matches(pat: MatchPat): Boolean =
        reset()
        matches(pat, vlu)

    /**
     * Matches the given pattern with the given value.
     *
     * @param pat
     *   the pattern to use for pattern maching
     * @param vlu
     *   the value to match against
     * @return
     *   true if the pattern matches the value. resolveBindings can be used to get a mapping between values and bindings
     */
    def matches(pat: MatchPat, vlu: ConcreteValues.Value): Boolean = pat match
        // TODO :think about lexical addresser
        // pat ::=
        // id                       matches anything and binds the value to id
        // (var id)                 same as id
        case IdPat(name, idn) =>
          checkDuplicate(name)
          bindings = bindings + (Identifier(name, idn) -> vlu)
          true
        // _                        matches anything
        case WildcardPat(_) =>
          true
        // (quote datum)            value equal? datum
        // TODO
        // (list lvp ...)
        // TODO
        // (list-rest lvp ... pat)
        // TODO
        // (list-no-order pat ...)
        // TODO
        // (list-no-order pat ... lvp)
        // TODO
        // (vector lvp ...)
        // TODO
        // (hash-table (pat pat) ...)
        // UNSUPPORTED
        // (hash-table (pat pat) ...+ ooo)
        // UNSUPPORTED
        // (cons pat pat)
        // TODO
        // (mcons pat pat)
        // UNSUPPORTED
        // (box pat)
        // UNSUPPORTED
        // (struct-id pat ...)
        // TODO
        // (struct struct-id (pat ...))
        // TODO
        // (regexp rx-expr)
        // UNSUPPORTED
        // (regexp rx-expr pat)
        // UNSUPPORTED
        // (pregexp px-expr)
        // UNSUPPORTED
        // (and pat ...)          value matches all the given patterns
        case AndPat(patterns) =>
          patterns.forall(matches(_, vlu))
        // (or pat ...)           matches if the value matches on of the patterns
        case OrPat(patterns) =>
          patterns.exists(matches(_, vlu))
        // (not pat ...)          matches if the value matches none of the patterns
        case NotPat(patterns) =>
          patterns.forall(!matches(_, vlu))
        // (app expr pats ...)
        case AppPat(expr, pats) =>
          assert(pats.size == 1, "unsupported: multiple return value (app pattern)")
          val res = eval(expr)
          pats.forall(matches(_, res))

        // (? expr pat ...)
        // TODO
        // (quasiquote qp)
        // UNSUPPORTED
        // derived-pattern
        // literal ::=
        // #t | #f | string | bytes(unsupported) | number | char | keyword(unsupported) | regexp(unsupported) | pregexp(unsupported)
        case LitPat(v) => matchesLiteral(v, vlu)
        // qp ::=
        // UNSUPPORTED
        // ooo ::=
        // UNSUPPORTED
        case _ => throw new Exception(s"unrecognized pattern $pat")

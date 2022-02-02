package maf.language.ContractScheme

import maf.language.sexp
import maf.language.scheme.*
import maf.core.{Expression, Identity}

sealed trait MatchPat:
    def fv: Set[String] = Set()
    def subexpressions: List[Expression] = List()

// id and (var id)
case class IdPat(name: String, idn: Identity) extends MatchPat
// _
case class WildcardPat(idn: Identity) extends MatchPat
// (quote datum)
case class QuotePat(vlu: sexp.SExp, idn: Identity) extends MatchPat
// (list lvp ...)
case class ListPat(pat: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pat.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pat.flatMap(_.subexpressions)

// (list-rest lvp ... pat) or (list* lvp ... pat)
case class ListRest(pat: List[MatchPat], rest: MatchPat) extends MatchPat:
    override def fv: Set[String] = rest.fv ++ pat.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pat.flatMap(_.subexpressions) ++ rest.subexpressions

// (list-no-order pat ...) or (list-no-order pat ... lvp)
case class ListNoOrderPat(pat: List[MatchPat], rest: Option[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pat.flatMap(_.fv).toSet ++ rest.map(_.fv).getOrElse(Set())
    override def subexpressions: List[Expression] = pat.flatMap(_.subexpressions) ++ rest.map(_.subexpressions).getOrElse(List())

// (vector lvp ...)
case class VectorPat(pat: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pat.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pat.flatMap(_.subexpressions)

// (cons pat pat )
case class ConsPat(car: MatchPat, cdr: MatchPat) extends MatchPat:
    override def fv: Set[String] = car.fv ++ cdr.fv
    override def subexpressions: List[Expression] = car.subexpressions ++ cdr.subexpressions

// (struct-id pat ...) or (struct struct-id (pat ...))
case class StructPat(tag: String, pats: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pats.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pats.flatMap(_.subexpressions)

// (and pat ...)
case class AndPat(pats: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pats.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pats.flatMap(_.subexpressions)

// (or pat ... )
case class OrPat(pats: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pats.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pats.flatMap(_.subexpressions)

// (not pat ...)
case class NotPat(pats: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = pats.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = pats.flatMap(_.subexpressions)

// (? expr pats ...)
case class PredPat(expr: SchemeExp, pats: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = expr.fv ++ pats.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = expr.subexpressions ++ pats.flatMap(_.subexpressions)

// (app expr pats ...)
case class AppPat(expr: SchemeExp, pats: List[MatchPat]) extends MatchPat:
    override def fv: Set[String] = expr.fv ++ pats.flatMap(_.fv).toSet
    override def subexpressions: List[Expression] = expr.subexpressions ++ pats.flatMap(_.subexpressions)

// lit
case class LitPat(vlu: sexp.Value) extends MatchPat

package maf.language.ContractScheme

import maf.language.sexp
import maf.language.scheme.*
import maf.core.Identity

sealed trait MatchPat

// id and (var id)
case class IdPat(name: String, idn: Identity) extends MatchPat
// _
case class WildcardPat(idn: Identity) extends MatchPat
// (quote datum)
case class QuotePat(vlu: sexp.SExp, idn: Identity) extends MatchPat
// (list lvp ...)
case class ListPat(pat: List[MatchPat]) extends MatchPat
// (list-rest lvp ... pat) or (list* lvp ... pat)
case class ListRest(pat: List[MatchPat], rest: MatchPat) extends MatchPat
// (list-no-order pat ...) or (list-no-order pat ... lvp)
case class ListNoOrderPat(pat: List[MatchPat], rest: Option[MatchPat]) extends MatchPat
// (vector lvp ...)
case class VectorPat(pat: List[MatchPat]) extends MatchPat
// (cons pat pat )
case class ConsPat(car: MatchPat, cdr: MatchPat) extends MatchPat
// (struct-id pat ...) or (struct struct-id (pat ...))
case class StructPat(tag: String, pats: List[MatchPat]) extends MatchPat
// (and pat ...)
case class AndPat(pats: List[MatchPat]) extends MatchPat
// (or pat ... )
case class OrPat(pats: List[MatchPat]) extends MatchPat
// (not pat ...)
case class NotPat(pats: List[MatchPat]) extends MatchPat
// (? expr pats ...)
case class PredPat(expr: SchemeExp, pats: List[MatchPat]) extends MatchPat
// (app expr pats ...)
case class AppPat(expr: SchemeExp, pats: List[MatchPat]) extends MatchPat
// lit
case class LitPat(vlu: sexp.Value) extends MatchPat

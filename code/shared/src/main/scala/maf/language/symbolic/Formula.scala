package maf.language.symbolic

import maf.language.scheme.*
import maf.core.*

/** A formula that can occur on the path condition */
sealed trait Formula:
    /** Returns the consistuents of the formula */
    def elements: List[Formula]

    /** Map a function over all the assertions in the formula. Returns a new formula where the assertions are mapped using the mapping function */
    def map(f: SchemeExp => SchemeExp): Formula

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula]

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula]

/** An assertion formed by constructing a scheme expression that can be interpreted as a boolean assertion */
case class Assertion(contents: SchemeExp) extends Formula:
    val elements: List[Formula] = List(this)
    override def toString = s"$contents"
    def map(f: SchemeExp => SchemeExp): Formula =
      Assertion(f(contents))

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula] = List(this)

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula] = List(this)

/**
 * A conjunction of two (or more) formulas
 *
 * Use the auxiliary <code>conj</code> function to create an instance of this class.
 *
 * @see
 *   maf.language.symbolic.FormulaAux.conj
 */
case class Conjunction(val elements: List[Formula]) extends Formula:
    override def toString: String = s"(${elements.mkString(" /\\ ")})"
    def map(f: SchemeExp => SchemeExp): Formula =
      Conjunction(elements.map(_.map(f)))

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula] = elements

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula] = List(this)

/**
 * A disjunction of two (or more) formulas
 *
 * Use the auxiliary <code>disj</code> function to create an instance of this class
 *
 * @see
 *   maf.language.symbolic.FormulaAux.disj
 */
case class Disjunction(val elements: List[Formula]) extends Formula:
    override def toString: String = s"(${elements.mkString(" \\/ ")})"
    def map(f: SchemeExp => SchemeExp): Formula =
      Disjunction(elements.map(_.map(f)))

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula] = List(this)

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula] = elements

/** Auxiliary functions */
object FormulaAux:

    /**
     * Helper function to construct a conjunction from a variable number of arguments
     *
     * @param as
     *   the formula(s) that will be combined into a conjunction
     */
    def conj(as: Formula*): Formula =
      conj(as.toList)

    /**
     * More generic form of <code>conj(Formula*)</code> but only accepts a list of formulas instead of a variable number of formulas
     *
     * @param as
     *   the list of formulas to combine into a conjunction
     * @param flatten
     *   true if the conjunction needs to be flattened (default)
     */
    def conj(as: List[Formula], flatten: Boolean = true): Formula =
      if as.size == 1 then as.head
      else if flatten then flatConj(as)
      else Conjunction(as.toSet.toList)

    /** Same as <code>conj</code> but constructs a disjunction instead */
    def disj(as: Formula*): Formula =
      disj(as.toList)

    /** Same as <code>conj</code> but constructs a disjunction instead */
    def disj(as: List[Formula], doFlat: Boolean = true): Formula =
      if as.size == 1 then as.head
      else if doFlat then flatten(as)
      else Disjunction(as.toList)

    /** Constructs an (isTrue? expr) expression */
    def isTrue(expr: SchemeExp): SchemeExp =
      SchemeFuncall(SchemeVar(Identifier("true?", Identity.none)), List(expr), Identity.none)

    /** Constructs an application (id vls ...) */
    def ap(id: SchemeExp, vls: SchemeExp*): SchemeExp =
      SchemeFuncall(id, vls.toList, Identity.none)

    /** Constructs an identifier (as a SchemeVar) from the given name */
    def id(name: String): SchemeExp =
      SchemeVar(Identifier(name, Identity.none))

    /** Constructs a number literal */
    def num(n: Int): SchemeExp =
      SchemeValue(maf.language.sexp.Value.Integer(n), Identity.none)

    def ass(assertion: SchemeExp): Formula =
      Assertion(assertion)

    /**
     * Flattens a list of conjunctions into a single conjunctions
     *
     * @param conjunctions
     *   the list of formulas that should occur in a conjunction
     */
    def flatConj(conjunctions: List[Formula]): Formula = conj(
      (conjunctions flatMap {
        case Conjunction(vs) =>
          flatConj(vs) match
              case Conjunction(vss) => vss
              case v                => List(v)
        case v => List(v)
      }).toSet.toList,
      false
    )

    /** Flatten a list of disjunctions into a single disjunctions */
    def flatten(disjunctions: List[Formula]): Formula = disj(
      (disjunctions flatMap {
        case Disjunction(djs) =>
          flatten(djs) match
              case Disjunction(rss) => rss
              case l                => List(l)

        case a @ Assertion(_) => List(disj(a))
        case v =>
          List(
            v
          ) // throw new Exception(s"only a list of disjunctions can be flattened, but got $v")
      }).toSet.toList,
      false
    )

    // From: https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists
    private def cartesianProduct[T](lst: List[T]*): List[List[T]] = {

      /**
       * Prepend single element to all lists of list
       * @param e
       *   single elemetn
       * @param ll
       *   list of list
       * @param a
       *   accumulator for tail recursive implementation
       * @return
       *   list of lists with prepended element e
       */
      def pel(e: T, ll: List[List[T]], a: List[List[T]] = Nil): List[List[T]] =
        ll match {
          case Nil     => a.reverse
          case x :: xs => pel(e, xs, (e :: x) :: a)
        }

      lst.toList match {
        case Nil      => Nil
        case x :: Nil => List(x)
        case x :: _ =>
          x match {
            case Nil => Nil
            case _ =>
              lst
                .foldRight(List(x))((l, a) => l.flatMap(pel(_, a)))
                .map(_.dropRight(x.size))
          }
      }
    }

    /** Distribute the a conjunction of disjunctions */
    def distribute(disjunctions: List[Formula]): Formula =
      disj(cartesianProduct(disjunctions.map(_.elements): _*).map(conj(_)))

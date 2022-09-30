package maf.language.symbolic

import maf.language.scheme.*
import maf.language.sexp.{Value => SValue}
import maf.core.*
import maf.core.Monad.*
import maf.core.Position.*
import maf.core.Monad.MonadSyntaxOps
import maf.core.Monad.MonadIterableOps

object Formula:
    def join(formulas: Formula*): Formula =
        import FormulaAux.*
        DNF.dnf(disj(formulas: _*))

/** A formula that can occur on the path condition */
sealed trait Formula:
    /** Returns the consistuents of the formula */
    def elements: Set[Formula]

    /** Map a function over all the assertions in the formula. Returns a new formula where the assertions are mapped using the mapping function */
    def mapOptionM[M[_]: Monad](f: SchemeExp => M[Option[SchemeExp]]): M[Option[Formula]]

    def mapOption(f: SchemeExp => Option[SchemeExp]): Option[Formula] =
        mapOptionM[IdentityMonad.Id](f)

    def map(f: SchemeExp => SchemeExp): Formula =
        mapOptionM[IdentityMonad.Id](e => Some(f(e))).get

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula]

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula]

    /** The number of assertions in the path condition */
    def size: Int

    /** Replace a particular symbolic expression with another one */
    def replace(changes: Map[SchemeExp, SchemeExp]): Formula

    /** Get a list of variables in the formula */
    def variables: List[String]

/** An empty formula */
case object EmptyFormula extends Formula:
    def elements: Set[Formula] = Set(this)

    def mapOptionM[M[_]: Monad](f: SchemeExp => M[Option[SchemeExp]]): M[Option[Formula]] = Monad[M].unit(Some(this))
    def splitConj: List[Formula] = List()
    def splitDisj: List[Formula] = List()
    def size: Int = 0
    def replace(changes: Map[SchemeExp, SchemeExp]): Formula = EmptyFormula
    def variables: List[String] = List()

/** A non-ground logical variable */
case class Var(id: Int) extends Formula:
    /** To use a variable inside a symbolic expression it has to be converted to an actual Scheme variable. */
    def symbolic: SchemeVar = Symbolic.LogicVar(id)
    override def toString: String = s"?$id"

    def elements: Set[Formula] = Set(this)
    def mapOptionM[M[_]: Monad](f: SchemeExp => M[Option[SchemeExp]]): M[Option[Formula]] = Monad[M].unit(Some(this))
    def splitConj: List[Formula] = List()
    def splitDisj: List[Formula] = List()
    def size: Int = 0
    def replace(changes: Map[SchemeExp, SchemeExp]): Formula = this
    def variables: List[String] = List()

trait SymbolicStoreM[M[_]] extends Monad[M]:
    def lookup(adr: Address): M[Symbolic.Symbolic]
    def store(adr: Address, s: Symbolic.Symbolic): M[Unit]

/** Monad to keep track of symbolic allocation and unifying variables */
trait SymbolicAllocator[M[_]] extends Monad[M]:
    /** Allocates a non-ground logical variable that can be unified with other variables */
    def freshVariable: M[Var]

    /** Creates a universally quantified variable */
    def fresh: M[Symbolic.Var]

    /**
     * Unifies the variable with the given formula
     *
     * @param v
     *   the variable to unify
     * @param formula
     *   the formula to unify the variable with.
     */
    def unify(v: Var, formula: Formula | SchemeExp): M[Unit]

    /** Retrieve the value of the given unified variable */
    def value(v: Var): M[Formula | SchemeExp]

    /** Convience value such that the monad operations work */
    implicit val self: Monad[M] = this

    /** Replace all logical variables with their unifications */
    def replaceAll(formula: Formula): M[Formula] =
        def replaceExp(e: SchemeExp): M[SchemeExp] = e match
            case Symbolic.Funcall(op, ops, idn) =>
                for
                    op_ <- replaceExp(op)
                    ops_ <- ops.mapM(replaceExp(_))
                yield Symbolic.Funcall(op_, ops_, idn)
            case v @ Symbolic.Value(_, _) => unit(v)
            case v @ Symbolic.Var(_)      => unit(v)
            case v @ Symbolic.LogicVar(id) =>
                value(Var(id)).map {
                    case e: SchemeExp => e
                    case _            => throw new Exception(s"cannot replace an expression with a formula for $v")
                }

        def replace(part: Formula): M[Formula] = part match
            case v: Var =>
                value(v).map {
                    case f: Formula => f
                    case _          => throw new Exception(s"cannot replace a formula with an expression for $v")
                }
            case Conjunction(es) =>
                es.mapM(replace).map(_.toSet).map(Conjunction.apply)
            case Disjunction(es) =>
                es.mapM(replace).map(_.toSet).map(Disjunction.apply)
            case Assertion(e) =>
                replaceExp(e) map Assertion.apply
            case e => unit(e)

        replace(formula)

/**
 * The symbolic assertion language is a subset of the actual scheme language.
 *
 * It features:
 *   - Function application
 *   - Literal values
 *   - Variables
 *   - Logical Variables
 *
 * It does not feature any special forms such as if, define, ...
 */
object Symbolic:
    type Symbolic = SchemeExp

    import maf.language.sexp.*
    import scala.util.control.TailCalls._

    export maf.language.scheme.{SchemeFuncall => Funcall, SchemeValue => Value, SchemeVar => Var}

    // Some extensions methods making building symbolic expressions easier
    extension (s: Symbolic)
        def ===(other: Symbolic): Symbolic =
            Funcall(VarId("="), List(s, other), Identity.none)

        /** Convience method to model application in the symbolic language */
        def apply(args: Symbolic*): Symbolic =
            Funcall(s, args.toList, Identity.none)

    /** Returns a set of symbolic variables (eg. identifiers starting with x) in the given symbolic expression. */
    def variables(sym: Symbolic): List[String] = sym match
        case Funcall(fexp, fargs, _) =>
            variables(fexp) ++ fargs.flatMap(variables)
        case VarId(s) if s.startsWith("x") => List(s)
        case _                             => List()

    type VarId = SchemeVar

    object VarId:
        def apply(id: String): SchemeVar = SchemeVar(Identifier(id, Identity.none))
        def unapply(other: Any): Option[String] =
            other match
                case SchemeVar(Identifier(name, _))       => Some(name)
                case SchemeVarLex(Identifier(name, _), _) => Some(name)
                case _                                    => None

    /**
     * A reference to a particular field of a struct.
     *
     * For example: forall m. m.tag = "display" where m.tag is a reference of id `m` and `tag` is the field. In S-expression syntax this would look
     * like: (ref m tag).
     */
    object Ref:
        def apply(id: SchemeVar, field: String): SchemeExp =
            SchemeFuncall(SchemeVar(Identifier("ref", Identity.none)), List(id, SchemeVar(Identifier(field, Identity.none))), Identity.none)

        def unapply(ref: SchemeExp): Option[(String, String)] = ref match
            case SchemeFuncall(SchemeVar(Identifier("ref", _)), List(SchemeVar(Identifier(id, _)), SchemeVar(Identifier(field, _))), _) =>
                Some((id, field))
            case _ => None

    /** Represents a logic variable, for example ?1 */
    object LogicVar:
        def apply(id: Int): SchemeVar = SchemeVar(Identifier(s"?$id", Identity.none))
        def unapply(vrr: SchemeExp): Option[(Int)] = vrr match
            case SchemeVar(Identifier(nam, _)) if nam.startsWith("?") =>
                Some(nam.split('?')(1).toInt)
            case _ => None

    /** A λ-abstraction, which is equivalent to a ∀, or can be instantiated with a specific expression */
    object Lam:
        def apply(x: Identifier, bdy: Symbolic): Symbolic =
            SchemeLambda(None, List(x), List(bdy), None, Identity.none)

        def unapply(lam: Symbolic): Option[(Identifier, Symbolic)] = lam match
            case SchemeLambda(_, List(x), List(bdy), None, _) => Some((x, bdy))
            case _                                            => None

    object Lit:
        /** Convience method such that literals can be created using Lit */
        def apply(s: Any): Symbolic = SchemeValue(
          (s match
              case s: String  => SValue.String(s)
              case i: Integer => SValue.Integer(BigInt(i))
              case r: Double  => SValue.Real(r)
              case b: Boolean => SValue.Boolean(b)
          ),
          Identity.none
        )

    object SymbolicCompiler extends BaseSchemeCompiler:
        override def _compile(exp: SExp): TailRec[SchemeExp] = exp match
            case SExpId(Identifier("□", _)) => done(□)
            case _                          => super._compile(exp)

    object Parser:
        def parse(s: String, tag: PTag = noTag): List[SchemeExp] = SExpParser.parse(s, tag).map(SymbolicCompiler.compile)

    /** An alias for a Hole. */
    def `□` : Symbolic = Hole(SchemeVar(Identifier("fresh", Identity.none)))
    def `□`(v: Symbolic): Symbolic = Hole(v)

    /**
     * A hole is a symbolic representation that must be later filled in with an actual fresh symbolic variable.
     *
     * Holes can have a rank, that can be used to identify multiple occurances of the same hole
     */
    object Hole:
        def unapply(v: SchemeExp): Option[(Symbolic)] = v match
            case SymbolicHole(v) => Some(v)
            case _               => None

        def apply(v: SchemeExp): SchemeExp =
            SymbolicHole(v)

    /**
     * The semantics of a hole is that it is "absorbing", which means that if it is joined with a particular symbolic expression (or assertion) it can
     * absorb matching parts of that expression (i.e. replace it with a hole).
     *
     * Example: □(+ x0 1) matches (+ (+ (+ x0 1) 1) 1) such that □ absorbs everything and (+ (+ x0 1) □) is returned.
     */

    /**
     * Checks whether the given assertion has a valid form
     *
     * @param vars
     *   a list of symbolic variables
     * @param ass
     *   the assertion to check
     * @return
     *   true if it is a valid assertion, false otherwise
     */
    def isValid(vars: List[String])(ass: SchemeExp): Boolean = ass match
        // Any assertion of the form (x e e) is valid, where x is not a symbolic variable
        case SchemeFuncall(SchemeVar(Identifier(id, _)), fargs, _) if !vars.contains(id) =>
            fargs.foldLeft(true)((acc, farg) => acc && isValid(vars)(farg))
        // Any identifier is a valid assertion
        case SchemeVar(_) => true
        // Any literal value is a valid assertion
        case SchemeValue(_, _) => true
        // A hole is a valid assertion
        case Hole(_) => true
        // Anything else is not a valid assertion
        case _ => false

    /** Strips any identity information from the given symbolic expression */
    def stripIdn(sym: Symbolic): Symbolic = sym match
        case SchemeFuncall(f, args, _)            => SchemeFuncall(f, args, Identity.none)
        case SchemeVar(Identifier(name, _))       => SchemeVar(Identifier(name, Identity.none))
        case SchemeVarLex(Identifier(name, _), _) => SchemeVar(Identifier(name, Identity.none))
        case SchemeValue(value, _)                => SchemeValue(value, Identity.none)

    /** Strips any identity information from the assertions in the formula */
    def stripIdn(formula: Formula): Formula =
        import FormulaAux.*
        formula match
            case Conjunction(cs) => conj(cs.map(stripIdn).toList: _*)
            case Disjunction(ds) => disj(ds.map(stripIdn).toList: _*)
            case Assertion(ass)  => Assertion(stripIdn(ass))
            case EmptyFormula    => EmptyFormula

/** An assertion formed by constructing a scheme expression that can be interpreted as a boolean assertion */
case class Assertion(contents: SchemeExp) extends Formula:
    val elements: Set[Formula] = Set(this)
    override def toString = s"$contents"

    def mapOptionM[M[_]: Monad](f: SchemeExp => M[Option[SchemeExp]]): M[Option[Formula]] =
        f(contents).map(_.map(Assertion(_)))

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula] = List(this)

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula] = List(this)

    def size: Int = 1

    def replace(changes: Map[SchemeExp, SchemeExp]) =
        def replaceContents(contents: SchemeExp): SchemeExp = contents match
            case SchemeFuncall(f, args, idn) => SchemeFuncall(replaceContents(f), args.map(replaceContents), idn)
            case e =>
                changes.get(e) match
                    case Some(to) => to
                    case _        => e

        Assertion(replaceContents(contents))

    def variables: List[String] = Symbolic.variables(contents)

/**
 * A conjunction of two (or more) formulas
 *
 * Use the auxiliary <code>conj</code> function to create an instance of this class.
 *
 * @see
 *   maf.language.symbolic.FormulaAux.conj
 */
case class Conjunction(val elements: Set[Formula]) extends Formula:
    override def toString: String = s"(${elements.mkString(" /\\ ")})"

    def mapOptionM[M[_]: Monad](f: SchemeExp => M[Option[SchemeExp]]): M[Option[Formula]] =
        elements.mapM(_.mapOptionM(f)).map(_.flatten).map(elms => if elms.size == 0 then None else Some(Conjunction(elms.toSet)))

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula] = elements.toList

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula] = List(this)

    def size: Int = elements.map(_.size).sum

    def replace(changes: Map[SchemeExp, SchemeExp]): Formula =
        Conjunction(elements.map(_.replace(changes)))

    def variables: List[String] = elements.flatMap(_.variables).toList

/**
 * A disjunction of two (or more) formulas
 *
 * Use the auxiliary <code>disj</code> function to create an instance of this class
 *
 * @see
 *   maf.language.symbolic.FormulaAux.disj
 */
case class Disjunction(val elements: Set[Formula]) extends Formula:
    override def toString: String = if elements.size >= 1 then s"(${elements.mkString(" \\/ ")})"
    else "(empty-or)"

    def mapOptionM[M[_]: Monad](f: SchemeExp => M[Option[SchemeExp]]): M[Option[Formula]] =
        elements.mapM(_.mapOptionM(f)).map(_.flatten).map(elms => if elms.size == 0 then None else Some(Disjunction(elms.toSet)))

    /** Split the formula into its constituents if it is a conjunction */
    def splitConj: List[Formula] = List(this)

    /** Split the formula into its constituents if it is a disjunction */
    def splitDisj: List[Formula] = elements.toList

    def size: Int = elements.map(_.size).sum

    def replace(changes: Map[SchemeExp, SchemeExp]): Formula = Disjunction(elements.map(_.replace(changes)))

    def variables: List[String] = elements.flatMap(_.variables).toList

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
        val asCleared = as.filterNot { _ == EmptyFormula }
        if asCleared.size == 0 then EmptyFormula
        else if asCleared.size == 1 then asCleared.head
        else if flatten then flatConj(asCleared.toSet)
        else Conjunction(asCleared.toSet)

    /** Same as <code>conj</code> but constructs a disjunction instead */
    def disj(as: Formula*): Formula =
        disj(as.toList)

    /** Same as <code>conj</code> but constructs a disjunction instead */
    def disj(as: List[Formula], doFlat: Boolean = true): Formula =
        val asCleared = as.filterNot { _ == EmptyFormula }
        if asCleared.size == 0 then EmptyFormula
        else if asCleared.size == 1 then asCleared.head
        else if doFlat then flatten(asCleared.toSet)
        else Disjunction(asCleared.toSet)

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
    def flatConj(conjunctions: Set[Formula]): Formula = conj(
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
    def flatten(disjunctions: Set[Formula]): Formula = disj(
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
        val res = disjunctions.map(_.elements)
        disj(cartesianProduct(disjunctions.map(_.elements.toList): _*).map(conj(_)))

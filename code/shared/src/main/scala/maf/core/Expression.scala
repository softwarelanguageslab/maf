package maf.core

import maf.util.SmartHash

/** An expression */
trait Expression extends SmartHash:

    /** The identity of the expression. Can be used to get information about the position of the expression in its source file. */
    def idn: Identity

    /** The set of free variables appearing in this expression. */
    def fv: Set[String]

    /** The height of the AST represented by this Scheme expression. */
    val height: Int = -1

    /** A label indicating the type of an expression. */
    def label: Label

    /** Returns a list of direct subexpressions of the given expression. */
    def subexpressions: List[Expression] // Uses `def` instead of `val` to avoid continuous memory overheads.

    /** Returns a list of all subexpressions of an expression. */
    def allSubexpressions: List[Expression] =
        var subExpressions = this.subexpressions
        var toExplore = subExpressions // Start with the direct subexpressions.
        while toExplore.nonEmpty do
            var sub = toExplore.flatMap(_.subexpressions)
            subExpressions = sub ::: subExpressions
            toExplore = sub
        subExpressions

    /**
     * Returns whether this expression is isomorphic to another expression. This is a basic implementation which should be specialised in subclasses.
     */
    def isomorphic(other: Expression): Boolean =
      label == other.label &&
        subexpressions.length == other.subexpressions.length &&
        subexpressions.zip(other.subexpressions).forall { case (x, y) => x.isomorphic(y) }

    /** Indicates whether this expression is equal to another expression when identity information of the expression is ignored. */
    def eql(other: Expression): Boolean = (hash == other.hash
      && label == other.label
      && subexpressions.length == other.subexpressions.length
      && subexpressions.zip(other.subexpressions).forall(p => p._1.eql(p._2)))

    /** A hash code that ignores positional information of the expression within the source code. */
    lazy val hash: Int = (label, subexpressions.map(_.hash)).hashCode()

object Expression:
    def eql(e1: Expression, e2: Expression): Boolean = e1.eql(e2)

/** An identifier. It has a name and a position */
case class Identifier(name: String, idn: Identity) extends Expression with SmartHash:
    def fullString: String = s"$name@$idn"
    override def toString: String = name
    def fv: Set[String] = Set(name)
    override val height: Int = 0
    val label: Label = Label.SYM
    def subexpressions: List[Expression] = List()
    override lazy val hash: Int = (label, name).hashCode()

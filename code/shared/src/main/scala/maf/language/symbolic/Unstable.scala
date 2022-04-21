package maf.language.symbolic

import maf.core.Identifier

/** Defines detection mechanisms for unstable sequences of symbolic expressions. See definition 3 in 4.4 in the paper. */
object Unstable:
    import Symbolic.*

    type Node = Symbolic
    type Tree = Symbolic

    /** Finds a path from the given node to the to the root node, and returns the subtrees associated with that path */
    private def findPath(from: Node, tree: Tree): List[Tree] =
        def findPathRec(tree: Tree): Option[List[Tree]] =
            if tree.isomorphic(from) then Some(List())
            else
                val childs = children(tree)
                children(tree).map(findPathRec).find(_.isDefined).getOrElse(None).map(tree :: _)

        findPathRec(tree).get.reverse

    /** Return the root of the given tree */
    private def root(tree: Tree): Node = tree

    /** Returns true if the givne tree is a strict subtree of the other tree */
    private def isSubTree(sub: Tree, parent: Tree): Boolean =
        def rec(sub: Tree, parent: Tree): Boolean =
            sub.isomorphic(parent) || children(parent).exists(rec(sub, _))

        children(parent).exists(rec(sub, _))

    /** Compute the number of nodes in the tree */
    def size(tree: Tree): Int =
        1 + children(tree).map(size).sum

    /** Returns a list of children for the given node */
    private def children(node: Node): List[Tree] =
        node match
            case Funcall(f, args, idn) => f :: args
            case Value(_, _)           => List()
            case Var(_)                => List()

    /** Given an unsorted sequence of trees, runs isUnstable on the sequence sorted by tree size */
    def isUnstableWithSort(sequence: List[Tree]): Boolean =
        isUnstable(sequence.sortBy(size))

    /** Returns true if an expression is isomorphic to the other */
    def isomorphic(a: Symbolic, b: Symbolic): Boolean = (a, b) match
        case (Funcall(fa, argsA, _), Funcall(fb, argsB, _)) =>
            isomorphic(fa, fb) && argsA.zip(argsB).forall(isomorphic)
        case (Value(va, _), Value(vb, _))                       => va == vb
        case (Var(Identifier(idA, _)), Var(Identifier(idB, _))) => idA == idB
        case _                                                  => false

    /** Returns true if the given sequence of expressions is unstable, it should be sorted according to tree size. */
    def isUnstable(sequence: List[Tree]): Boolean =
        sequence.size >= 2 && /* the sequence must have at least two elements */
            /** Each tree must have more than 1 node */
            sequence.forall(size(_) > 1) &&
            isSubTree(sequence.head, sequence.last) /* the first and last element should be subtrees of each other */ && {
                val path = findPath(sequence.head, sequence.last)
                path.zip(sequence).forall { case (root, expr) =>
                    children(root).exists(isomorphic(_, expr))
                }
                /* each node on the path should have the previous expression in the sequence as its child */
            }

package maf.language.symbolic

/** Defines detection mechanisms for unstable sequences of symbolic expressions. See definition 3 in 4.4 in the paper. */
object Unstable:
    import Symbolic.*

    type Node = Symbolic
    type Tree = Symbolic

    /** Finds a path from the given node to the to the root node, and returns the subtrees associated with that path */
    private def findPath(from: Node, tree: Tree): List[Tree] = ???

    /** Return the root of the given tree */
    private def root(tree: Tree): Node = ???

    /** Returns true if the givne tree is a subtree of the other tree */
    private def isSubTree(sub: Tree, parent: Tree): Boolean = ???

    /** Returns a list of children for the given node */
    private def children(node: Node): List[Tree] = ???

    /** Given an unsorted sequence of trees, runs isUnstable on the sequence sorted by tree size */
    def isUnstableWithSort(sequence: List[Symbolic]): Boolean = ???

    /** Returns true if the given sequence of expressions is unstable, it should be sorted according to tree size. */
    def isUnstable(sequence: List[Symbolic]): Boolean =
        sequence.size >= 2 && /* the sequence must have at least two elements */
            isSubTree(sequence.head, sequence.last) /* the first and last element should be subtrees of each other */ && {
                val path = findPath(sequence.head, sequence.last)
                path.zip(sequence).forall { case (root, expr) => children(root).exists(_.isomorphic(expr)) }
                /* each node on the path should have the previous expression in the sequence as its child */
            }

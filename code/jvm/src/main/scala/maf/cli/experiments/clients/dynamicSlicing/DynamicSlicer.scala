package maf.cli.experiments.clients.dynamicSlicing

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.ModAnalysis
import maf.core.Identifier
import maf.language.scheme.SchemeExp
import maf.core.Identity

case class DynamicNode(reachableStmts: Set[DynamicNode], // reachableStmts maps a node to the set of all statements that can be reached from the given node
                       exp: SchemeExp, // the expression that this node belongs to
                       descendants: Set[DynamicNode]) //descendants are the direct descendants of the node
    
trait DynamicSlicer extends BigStepModFSemanticsT:
    // defnNode maps a variable name to the node in the graph that last assigned a value to that variable
    var defnNode: Map[Identifier, DynamicNode] = Map.empty
    // predNode maps a control predicate statement to the node that corresponds to the last occurrence of
    // this predicate in the execution history thus far
    // TODO: should be kept in the EvalM monad
    var predNode: Map[Identity, DynamicNode] = Map.empty
    var nodes: Set[DynamicNode]

    def findNode(node: DynamicNode): Option[DynamicNode] =
        nodes.find(n => n.exp == node.exp && n.descendants == node.descendants)

    def mergeNodes(oldNode: DynamicNode, newNode: DynamicNode): DynamicNode = 
        DynamicNode(oldNode.reachableStmts ++ newNode.reachableStmts, 
                    oldNode.exp, 
                    oldNode.descendants ++ newNode.descendants)
        
    override def intraAnalysis(cmp: Component): DynamicSlicerIntra 

    trait DynamicSlicerIntra extends IntraAnalysis with BigStepModFIntraT: 
        override def eval(exp: SchemeExp): EvalM[Value] = 
            // D: the set of nodes that last assigned values to the variables used by the expression
            val D: List[Option[DynamicNode]] = exp.usedSet().map(defnNode.get)
            // C: the control predicate node of the statement
            // TODO: change the EvalM monad to keep track of this
            val C: Option[DynamicNode] = None
   
            val descs = (C :: D).flatten.toSet
            val reachable = descs.flatMap(_.reachableStmts)
            val node =  DynamicNode(reachable, exp, descs)

            def updateDefnNode(node: DynamicNode) = 
                for(identifier <- exp.definedSet()) {
                    defnNode = defnNode + (identifier -> node)
                }

            findNode(node) match 
                // if there already is a node for this expression with the same descendants, check the reachablestmts
                case Some(n) =>
                    if !(reachable subsetOf n.reachableStmts) then 
                        nodes = nodes + node
                        updateDefnNode(node)
                    else // otherwise, merge the old node with the new one
                        nodes = nodes - n
                        val newNode = mergeNodes(n, node)
                        nodes = nodes + newNode
                        updateDefnNode(newNode)
                // if there is no node yet, we make a new one
                case None => 
                    nodes = nodes + node  
                    updateDefnNode(node)

            super.eval(exp)

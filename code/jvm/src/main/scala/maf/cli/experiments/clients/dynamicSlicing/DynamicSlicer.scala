package maf.cli.experiments.clients.dynamicSlicing

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.ModAnalysis
import maf.core.Identifier
import maf.language.scheme.SchemeExp
import maf.core.Identity

import maf.modular.scheme.modf.SchemeModFSemanticsM
import maf.modular.scheme.modf.BigStepModFSemantics
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.language.symbolic.lattices.SymbolicSchemeConstantPropagationDomain
import maf.modular.scheme.modf.TEvalM

import maf.core._
import maf.core.Monad.MonadSyntaxOps
import maf.core.Position._
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout
import maf.cli.experiments.clients.dynamicSlicing.TControlEvalM.ControlEvalM
import maf.cli.experiments.clients.dynamicSlicing.TControlEvalM.MonadControlEvalM
import maf.cli.experiments.clients.dynamicSlicing.DynamicNode
import maf.language.scheme.LexicalRef
import maf.modular.scheme.modflocal.SchemeSemantics

case class DynamicNode(id: Int,
                       reachableStmts: Set[DynamicNode], // reachableStmts maps a node to the set of all statements that can be reached from the given node
                       exp: SchemeExp, // the expression that this node belongs to
                       descendants: Set[DynamicNode]) //descendants are the direct descendants of the node

// a version of EvalM that saves the last control node passed
object TControlEvalM: 
                                     // environment, control node, current node, map of addresses to the set of last definition sites
    case class ControlEvalM[+X](run: (Environment[Address], Option[DynamicNode], Option[DynamicNode], Map[Address, Set[DynamicNode]]) => Option[(X, Map[Address, Set[DynamicNode]])]):
       def flatMap[Y](f: X => ControlEvalM[Y]): ControlEvalM[Y] = ControlEvalM((env, ctrlNode, currNode, defs) => run(env, ctrlNode, currNode, defs).flatMap((res, newDefs) => f(res).run(env, ctrlNode, currNode, newDefs)))
       def map[Y](f: X => Y): ControlEvalM[Y] = ControlEvalM((env, ctrlNode, currNode, defs) => run(env, ctrlNode, currNode, defs).map((res, newDefs) => (f(res), newDefs))) 

    trait MonadControlEvalM extends TEvalM[ControlEvalM]:
        def map[X, Y](m: ControlEvalM[X])(f: X => Y): ControlEvalM[Y] = m.map(f)
        def flatMap[X, Y](m: ControlEvalM[X])(f: X => ControlEvalM[Y]): ControlEvalM[Y] = m.flatMap(f)
        def unit[X](x: X): ControlEvalM[X] = ControlEvalM((_, _, _, defs) => Some(x, defs))
        def mzero[X]: ControlEvalM[X] = ControlEvalM((_, _, _, _) => None)
        implicit class MonadicOps[X](xs: Iterable[X]):
            def foldLeftM[Y](y: Y)(f: (Y, X) => ControlEvalM[Y]): ControlEvalM[Y] = xs match
                case Nil     => unit(y)
                case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
            def mapM[Y](f: X => ControlEvalM[Y]): ControlEvalM[List[Y]] = xs match
                case Nil => unit(Nil)
                case x :: xs =>
                    for
                        fx <- f(x)
                        rest <- xs.mapM(f)
                    yield fx :: rest
            def mapM_(f: X => ControlEvalM[Unit]): ControlEvalM[Unit] = xs match
                case Nil     => unit(())
                case x :: xs => f(x).flatMap(_ => xs.mapM_(f))  
        def getEnv: ControlEvalM[Environment[Address]] = ControlEvalM((env, _, _, defs) => Some(env, defs))
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => ControlEvalM[X]): ControlEvalM[X] = 
            ControlEvalM((env, ctrlNode, currNode, defs) => ev.run(f(env), ctrlNode, currNode, defs))  
        def merge[X: Lattice](x: ControlEvalM[X], y: ControlEvalM[X]): ControlEvalM[X] = ControlEvalM { (env, ctrlNode, currNode, defs) =>
            (x.run(env, ctrlNode, currNode, defs), y.run(env, ctrlNode, currNode, defs)) match
                case (None, yres)             => yres
                case (xres, None)             => xres
                case (Some((res1, defs1)), Some((res2, defs2))) => Some((Lattice[X].join(res1, res2), defs1 ++ defs2))
        }
        def fail[X](err: Error): ControlEvalM[X] = mzero
        // ADDED FOR CONTROLNODE
        def getControlNode: ControlEvalM[Option[DynamicNode]] = ControlEvalM((_, ctrlNode, currNode, defs) => Some(ctrlNode, defs))
        def pushControlNode[X](node: DynamicNode)(ev: => ControlEvalM[X]): ControlEvalM[X] = 
            // add it as the current control node but also as the current node
            ControlEvalM((env, _, currNode, defs) => ev.run(env, Some(node), Some(node), defs)) 
        def pushControlNodeM[X](node: DynamicNode)(ev: ControlEvalM[X]): ControlEvalM[X] =
            given Monad[ControlEvalM] = this 
            for 
                result <- pushControlNode(node) { ev } 
            yield result
        // ADDED FOR CURRENTNODE
        def getCurrentNode: ControlEvalM[Option[DynamicNode]] = ControlEvalM((_, ctrlNode, currNode, defs) => Some(currNode, defs))
        def pushCurrentNode[X](node: DynamicNode)(ev: => ControlEvalM[X]): ControlEvalM[X] = 
            ControlEvalM((env, ctrlNode, currNode, defs) => ev.run(env, ctrlNode, Some(node), defs)) 
        def pushCurrentNodeM[X](node: DynamicNode)(ev: ControlEvalM[X]): ControlEvalM[X] =
            given Monad[ControlEvalM] = this 
            for 
                result <- pushCurrentNode(node) { ev } 
            yield result
        // ADDED FOR DEFS
        def addDef(address: Address, node: Option[DynamicNode]): ControlEvalM[Unit] = 
            ControlEvalM((env, ctrlNode, currNode, defs) => Some((), defs + (address -> Set(node.get)))) // todo: initial definition 


trait DynamicSlicer extends BigStepModFSemanticsT:
    import TControlEvalM.{*}

    object ControlEvalM extends MonadControlEvalM

    override type EvalM[X] = ControlEvalM[X] 
    implicit val evalM = ControlEvalM
    val controlEvalM: MonadControlEvalM = ControlEvalM 

    var lastId = -1


    // defnNode maps a variable name to the node in the graph that last assigned a value to that variable
    var defnNode: Map[Identifier, DynamicNode] = Map.empty
    // var defnNode: Map[String, DynamicNode] = Map.empty
    var nodes: Set[DynamicNode] = Set.empty

    def findNode(node: DynamicNode): Option[DynamicNode] =
        nodes.find(n => n.exp == node.exp && n.descendants == node.descendants)

    def mergeNodes(oldNode: DynamicNode, newNode: DynamicNode): DynamicNode = 
        lastId = lastId + 1
        DynamicNode(lastId,
                    oldNode.reachableStmts ++ newNode.reachableStmts, 
                    oldNode.exp, 
                    oldNode.descendants ++ newNode.descendants)
        
    override def intraAnalysis(cmp: Component): DynamicSlicerIntra 

    trait DynamicSlicerIntra extends IntraAnalysis with BigStepModFIntraT: 
        import controlEvalM._

        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(fnEnv, None, None, Map.empty).foreach((res, defs) => 
                println(defs)
                writeResult(res))

        override def eval(exp: SchemeExp): ControlEvalM[Value] = 

            def updateDefnNode(node: DynamicNode) = 
                // TODO: defnNode should be the specific binding 
                for(identifier <- exp.definedSet()) {
                    defnNode = defnNode + (identifier -> node)
                    // defnNode = defnNode + (identifier.toString -> node)
                }

            def addNode(node: DynamicNode) = 
                findNode(node) match 
                // if there already is a node for this expression with the same descendants, check the reachablestmts
                case Some(n) =>
                    if !(n.reachableStmts subsetOf node.reachableStmts) then 
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

            // C: the control predicate node of the statement
            getControlNode.flatMap(c => 
                // println("exp: " + exp)
                // println("defnnode: " + defnNode.keySet)
                // D: the set of nodes that last assigned values to the variables used by the expression
                val d: List[Option[DynamicNode]] = exp.definedSet().map(defnNode.get)
                // println("d: " + d.map(_.map(_.id)))
                val descs = (c :: d).flatten.toSet
                val reachable = descs.flatMap(_.reachableStmts) ++ descs
                lastId = lastId + 1
                val node = DynamicNode(lastId, reachable, exp, descs)
                addNode(node)
                // push the node if this is a control node
                exp match
                    case SchemeIf(cond, cons, alt, _) => 
                        pushControlNodeM(node)(super.evalIf(cond, cons, alt))
                    case _ => pushCurrentNodeM(node)(super.eval(exp))
                
            )
        override protected def bind(
            id: Identifier,
            env: Env,
            vlu: Value
          ): M[Env] =
            getCurrentNode.flatMap(node =>
                val addr = allocVar(id, component)
                val env2 = env.extend(id.name, addr)
                for 
                    _ <- addDef(addr, node)
                    _ <- write(addr, vlu)
                    env <- baseEvalM.unit(env2)
                yield env
                )
            

        
object DynamicSlicer:
    type Analysis = DynamicSlicer

    def createAnalysis(program: SchemeExp): DynamicSlicer =
        new ModAnalysis[SchemeExp](program)
            with StandardSchemeModFComponents
            with SchemeModFSemanticsM
            with SchemeModFNoSensitivity
            with SymbolicSchemeConstantPropagationDomain
            with FIFOWorklistAlgorithm[SchemeExp]
            with DynamicSlicer:

            class AnalysisIntra(cmp: Component) extends IntraAnalysis(cmp) with DynamicSlicerIntra
            override def intraAnalysis(cmp: Component): AnalysisIntra =
                new AnalysisIntra(cmp)
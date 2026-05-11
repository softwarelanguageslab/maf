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
import maf.language.symbolic.EmptyFormula.variables

case class DynamicNode(id: Int,
                       exp: Object,
                       dependencies: Set[Int]) 

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
        def pushControlNode[X](node: Option[DynamicNode])(ev: => ControlEvalM[X]): ControlEvalM[X] = 
            // add it as the current control node but also as the current node
            ControlEvalM((env, _, currNode, defs) => ev.run(env, node, node, defs)) 
        def pushControlNodeM[X](node: Option[DynamicNode])(ev: ControlEvalM[X]): ControlEvalM[X] =
            given Monad[ControlEvalM] = this 
            for 
                result <- pushControlNode(node) { ev } 
            yield result
        // ADDED FOR CURRENTNODE
        def getCurrentNode: ControlEvalM[Option[DynamicNode]] = ControlEvalM((_, _, currNode, defs) => Some(currNode, defs))
        def pushCurrentNode[X](node: Option[DynamicNode])(ev: => ControlEvalM[X]): ControlEvalM[X] = 
            ControlEvalM((env, ctrlNode, currNode, defs) => ev.run(env, ctrlNode, node, defs)) 
        def pushCurrentNodeM[X](node: Option[DynamicNode])(ev: ControlEvalM[X]): ControlEvalM[X] =
            given Monad[ControlEvalM] = this 
            for 
                result <- pushCurrentNode(node) { ev } 
            yield result
        // ADDED FOR DEFS
        def addDef(address: Address, node: Option[DynamicNode]): ControlEvalM[Unit] = 
            ControlEvalM((env, ctrlNode, currNode, defs) => 
                // println("adding def: " + address + " " + node.get.id + " " + node.get.index)
                val newDefs = defs + (address -> Set(node.get))
                Some((), newDefs)) // todo: keep initial definition 
        def getDefs: ControlEvalM[Map[Address, Set[DynamicNode]]] = ControlEvalM((_, _, _, defs) => Some(defs, defs))

trait DynamicSlicer extends BigStepModFSemanticsT:
    import TControlEvalM.{*}

    object ControlEvalM extends MonadControlEvalM

    override type EvalM[X] = ControlEvalM[X] 
    implicit val evalM = ControlEvalM
    val controlEvalM: MonadControlEvalM = ControlEvalM 

    var lastId = -1

    var nodes: Set[DynamicNode] = Set.empty

    def findNode(node: DynamicNode): Option[DynamicNode] =
        nodes.find(n => n.exp.toString() == node.exp.toString())
        
    def mergeNodes(oldNode: DynamicNode, newNode: DynamicNode): DynamicNode = 
        DynamicNode(oldNode.id, 
                    oldNode.exp, 
                    oldNode.dependencies ++ newNode.dependencies)

    def addNodeBinding(exp: (Identifier, SchemeExp), deps: Set[Int]) =
        exp match
            case Tuple2(_, e) => 
                if e.isPrimitive then 
                    None
                else 
                    addNodeObject(exp, deps)

    def addNodeExp(exp: SchemeExp, deps: Set[Int]) = 
        if exp.isPrimitive then 
            None 
        else 
            addNodeObject(exp, deps)
        
    def addNodeObject(exp: Object, deps: Set[Int]): Option[DynamicNode] =
        lastId = lastId + 1 
        val node = DynamicNode(lastId, exp, deps)
        findNode(node) match 
        // if there already is a node, merge the old and the new
        case Some(n) =>
            nodes = nodes - n
            val newNode = mergeNodes(n, node)
            nodes = nodes + newNode
            Some(newNode)
        // if there is no node yet, we make a new one
        case None => 
            nodes = nodes + node
            Some(node)


    override def intraAnalysis(cmp: Component): DynamicSlicerIntra 
    trait DynamicSlicerIntra extends IntraAnalysis with BigStepModFIntraT: 
        import controlEvalM._

        def analyzeWithTimeout(timeout: Timeout.T): Unit = // Timeout is just ignored here.
            eval(fnBody).run(fnEnv, None, None, Map.empty).foreach((res, defs) => 
                println("defs:")
                defs.map((adr, vals) => 
                    print("    " + adr + " nodes: ")
                    vals.map(v => print(v.id + ", "))
                    println())
                writeResult(res))

        override def eval(exp: SchemeExp): ControlEvalM[Value] = 
            val dependencies = exp.subexpressions.filter(e => e != exp).flatMap(subexp =>
                addNodeObject(subexp, Set.empty))
            println(exp)
            print("made nodes for subexpressions: ")
            dependencies.map(dep => print(dep.id + " "))
            println()
            println()
            getControlNode.flatMap(c => 
                getDefs.flatMap{ defnNode =>
                    exp match
                        case SchemeVarLex(id, _) => 
                            for 
                                adr <- getEnv.flatMap(env => baseEvalM.unit(env.lookup(id.name)))
                                d = defnNode.getOrElse(adr.get, Set.empty)
                                // _ = print("d: ")
                                // _ = d.map(ds => print(ds.id + " "))
                                // _ = println()
                                deps = (d ++ c) ++ dependencies
                                node = addNodeExp(exp, deps.map(_.id))
                                // _ = println("^ node: " + node.get.id)
                                res <- pushCurrentNodeM(node)(super.eval(exp))
                            yield res
                        case SchemeIf(cond, cons, alt, _) => // this node is a control node
                            val deps = c.toSet ++ dependencies
                            val node = addNodeExp(exp, deps.map(_.id))
                            pushControlNodeM(node)(super.evalIf(cond, cons, alt))
                        case _ => 
                            val deps = c.toSet ++ dependencies
                            val node = addNodeExp(exp, deps.map(_.id))
                            pushCurrentNodeM(node)(super.eval(exp))
                            
                            
                
            })
        protected def bind(
            id: Identifier,
            env: Env,
            vlu: Value,
            index: Int, //the index of the binding so that we can make the correct node
            boundNode: DynamicNode // the (future) node of the rhs
          ): M[Env] =
            getCurrentNode.flatMap(node =>
                // println("binding: " + id + " index: " + index)
                val addr = allocVar(id, component)
                val env2 = env.extend(id.name, addr)
                val newExp = node.get.exp match
                    case SchemeLet(bindings, _, _) => bindings(index)
                    case SchemeLetStar(bindings, _, _) => bindings(index)
                    case SchemeLetrec(bindings, _, _) => bindings(index)
                    case _ => node.get.exp
                val deps = Set(node.get, boundNode).map(_.id)
                val newNode = 
                    newExp match
                        case e: (Identifier, SchemeExp) => addNodeBinding(e, deps)
                        case e: SchemeExp => addNodeExp(e, deps)
                for 
                    _ <- newNode match
                        case Some(n) => addDef(addr, Some(n))
                        case None => baseEvalM.unit(())
                    _ <- write(addr, vlu)
                    env <- baseEvalM.unit(env2)
                yield env
                )
            
        protected def bind(bds: List[(Identifier, Value)], env: Env, boundNodes: List[DynamicNode]): M[Env] =
            bds.zipWithIndex.foldLeftM(env)((env2, bnd) => 
                bind(bnd._1._1, env2, bnd._1._2, bnd._2, boundNodes(bnd._2)))

        override protected def evalLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            var boundNodes: List[DynamicNode] = List.empty
            for
                bds <- bindings.mapM { case (id, exp) => 
                    val boundNode = addNodeExp(exp, Set.empty).get
                    boundNodes = boundNode :: boundNodes
                    eval(exp).map(vlu => (id, vlu)) }
                res <- withEnvM(env => bind(bds, env, boundNodes)) {
                    evalSequence(body)
                }
            yield res

        override protected def evalLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            evalLetStarIndex(bindings, body, 0)
        protected def evalLetStarIndex(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], index: Int): EvalM[Value] =
            bindings match
                case Nil => evalSequence(body)
                case (id, exp) :: restBds =>
                    eval(exp).flatMap { rhs =>
                        val boundNode = addNodeExp(exp, Set.empty).get
                        withEnvM(env => bind(id, env, rhs, index, boundNode)) {
                            evalLetStarIndex(restBds, body, index + 1)
                        }
                    }

        override protected def evalLetRec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp]): EvalM[Value] =
            withEnvM(env => bindings.zipWithIndex.foldLeftM(env) { case (env2, ((id, exp), index)) => 
                val boundNode = addNodeExp(exp, Set.empty).get
                bind(id, env2, lattice.bottom, index, boundNode) }) {
                for
                    extEnv <- getEnv
                    _ <- bindings.mapM_ { case (id, exp) =>
                        eval(exp).flatMap(value => assign(id, extEnv, value))
                    }
                    res <- evalSequence(body)
                yield res
            }


        
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
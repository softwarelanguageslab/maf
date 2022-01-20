package maf.modular.scv

import maf.language.scheme.*
import maf.language.ContractScheme.ContractValues.*
import scala.reflect.ClassTag
import maf.language.ContractScheme.StructOps

/**
 * Adds support for structures in ContractScheme programs.
 *
 * The program must first be compiled using the ContractScheme compiler for the structures to be recognized
 */
trait ScvWithStructs extends BaseScvBigStepSemantics:
    /** ClassTag is required for the construction of a fields array at run time */
    protected val valueClassTag: ClassTag[Value]

    override def intraAnalysis(component: Component): IntraScvSemanticsWithStructs

    trait IntraScvSemanticsWithStructs extends BaseIntraScvSemantics:
        import evalM._

        private given ClassTag[Value] = valueClassTag

        private val structOps = StructOps()

        override def eval(exp: SchemeExp): ScvEvalM[Value] = exp match
            // (_make_constr tag siz)
            case mk: MakeStruct =>
              unit(structOps.evaluate(mk))

            case _ => super.eval(exp)

        override def callFun(f: PostValue, args: List[PostValue]): ScvEvalM[Value] =
          // the result of the computation diverges if the abstract value is more than one of the above.
          // to avoid a loss of precision, a widening operation is not performed.
          nondets(structOps.call(f.value, args.map(_.value)))

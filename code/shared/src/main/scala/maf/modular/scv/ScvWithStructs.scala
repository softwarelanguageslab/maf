package maf.modular.scv

import maf.language.scheme.*
import maf.language.ContractScheme.ContractValues.*
import scala.reflect.ClassTag

/**
 * Adds support for structures in ContractScheme programs.
 *
 * The program must first be compiled using the ContractScheme compiler for the structures to be recognized
 */
trait ScvWithStructs extends ScvBigStepSemantics:
    given valueClassTag: ClassTag[Value]

    override def intraAnalysis(component: Component): IntraScvSemanticsWithStructs

    trait IntraScvSemanticsWithStructs extends IntraScvSemantics:
        import evalM._
        override def eval(exp: SchemeExp): ScvEvalM[Value] = exp match
            // (_make_constr tag siz)
            case MakeStructConstr(tag, siz, _) =>
              unit(lattice.structConstructor(StructConstructor(tag, siz)))

            // (_make_struct_getter tag idx)
            case MakeStructGetter(tag, idx, _) =>
              unit(lattice.structSetterGetter(StructSetterGetter(tag, idx, false)))

            // (_make_struct_setter tag idx)
            case MakeStructSetter(tag, idx, _) =>
              unit(lattice.structSetterGetter(StructSetterGetter(tag, idx, true)))

            case _ => super.eval(exp)

        override def callFun(f: PostValue, args: List[PostValue]): ScvEvalM[Value] =
            // We provide support for applying constructors, setters and getters
            val constrs: Set[EvalM[Value]] = lattice.getStructConstructor(f.value).map { constr =>
                assert(args.size == constr.size)
                val newStruct = Struct(constr.tag, Array.from(args.map(_.value)))
                unit(lattice.struct(newStruct))
            }

            // A setter destructively updates the value itself. This encodes the same global store semantics as the entire analysis. However, it might be better to use an address for the fields in the struct instead, and always refer to the global store for updating/lookup (TODO).
            val setters: Set[EvalM[Value]] = lattice.getGetterSetter(f.value).filter(_.isSetter).flatMap { setter =>
                assert(args.size == 2)
                val oldStruct = args(0)._2
                lattice.getStructs(oldStruct).map { struct =>
                    val newValue = args(1)._2
                    struct.fields(setter.idx) = lattice.join(struct.fields(setter.idx), newValue)
                    unit(lattice.nil)
                }
            }

            // A getter must read the value on the specified index from the field array of the struct
            val getters: Set[EvalM[Value]] = lattice.getGetterSetter(f.value).filterNot(_.isSetter).flatMap { getter =>
                assert(args.size == 1)
                val struct = args(0)._2
                lattice.getStructs(struct).map { struct =>
                    val value = struct.fields(getter.idx)
                    unit(value)
                }
            }

            // the result of the computation diverges if the abstract value is more than one of the above.
            // to avoid a loss of precision, a widening operation is not performed.
            nondets(constrs ++ setters ++ getters)

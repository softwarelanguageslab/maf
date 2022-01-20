package maf.language.ContractScheme

import maf.language.scheme.lattices.*
import maf.core.Address
import maf.language.scheme.*
import maf.language.scheme.MakeStructGetter
import maf.language.ContractScheme.ContractValues.*
import maf.language.scheme.primitives.SchemePrimM
import scala.annotation.meta.getter
import scala.reflect.ClassTag
import maf.core.Error

case class StructInvalidArity(name: String, expected: Int, got: Int) extends Error

/**
 * Semantics for creating and refering to structs.
 *
 * It implements the semantics for creating setters, getters and constructors as well as applying these setters, getters and constructors.
 */
class StructOps[Value](using lat: SchemeLattice[Value, Address], clsT: ClassTag[Value]):
    /** Can be inserted in the evaluation process to evaluate one of the expressions related to structs to values */
    def evaluate(exp: MakeStruct): Value =
      exp match
          case MakeStructConstr(tag, siz, _) =>
            lat.structConstructor(StructConstructor(tag, siz))

          // (_make_struct_getter tag idx)
          case MakeStructGetter(tag, idx, _) =>
            lat.structSetterGetter(StructSetterGetter(tag, idx, false))

          // (_make_struct_setter tag idx)
          case MakeStructSetter(tag, idx, _) =>
            lat.structSetterGetter(StructSetterGetter(tag, idx, true))

          // (make_struct_predicate tag)
          case MakeStructPredicate(tag, _) =>
            lat.structPredicate(StructPredicate(tag))

    /** Call a setter, getter or constructor */
    def call[M[_]](op: Value, args: List[Value])(using monad: SchemePrimM[M, Address, Value]): Set[M[Value]] =
        // We provide support for applying constructors, setters and getters
        val constrs: Set[M[Value]] = lat.getStructConstructor(op).map { constr =>
            assert(args.size == constr.size)
            val newStruct = Struct(constr.tag, maf.util.ArrayEq.from(args))
            monad.unit(lat.struct(newStruct))
        }

        // A setter destructively updates the value itself. This encodes the same global store semantics as the entire analysis. However, it might be better to use an address for the fields in the struct instead, and always refer to the global store for updating/lookup (TODO).
        val setters: Set[M[Value]] = lat.getGetterSetter(op).filter(_.isSetter).flatMap { setter =>
            assert(args.size == 2)
            val oldStruct = args(0)
            lat.getStructs(oldStruct).map { struct =>
                val newValue = args(1)
                struct.fields.update(setter.idx, lat.join(struct.fields(setter.idx), newValue))
                monad.unit(lat.nil)
            }
        }

        // A getter must read the value on the specified index from the field array of the struct
        val getters: Set[M[Value]] = lat.getGetterSetter(op).filterNot(_.isSetter).flatMap { getter =>
            assert(args.size == 1)
            val struct = args(0)
            lat.getStructs(struct).map { struct =>
                val value = struct.fields(getter.idx)
                monad.unit(value)
            }
        }

        // A predicate must check the type tag of the struct
        val predicates: Set[M[Value]] = lat.getStructPredicates(op).flatMap { pred =>
            assert(args.size == 1)
            val structArg = args(0)
            lat.getStructs(structArg).map { struct =>
              monad.unit(lat.bool(struct.tag == pred.tag))
            }
        }

        constrs ++ setters ++ getters ++ predicates

object StructOps:
    def unapply(that: Any): Option[MakeStruct] =
      that match
          case ms: MakeStruct => Some(ms)
          case _              => None

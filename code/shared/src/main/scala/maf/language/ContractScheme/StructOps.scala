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
          case MakeStructGetter(tag, idx, _) =>
            lat.structSetterGetter(StructSetterGetter(tag, idx, false))

          case MakeStructSetter(tag, idx, _) =>
            lat.structSetterGetter(StructSetterGetter(tag, idx, true))

          case MakeStructConstr(tag, siz, _) => ???
          case _                             => lat.bottom

    def getConstructor(v: Value): Set[(String, Int)] = ???

    /** Call a setter, getter or constructor */
    def call[M[_]](op: Value, args: List[Value])(using monad: SchemePrimM[M, Address, Value]): M[Value] =
        val gettersSetters: Set[M[Value]] =
          lat.getGetterSetter(op).flatMap { getterSetter =>
            if getterSetter.isSetter then
                // Setter (set-posn-x! inst val)
                if args.size != 2 then Set(monad.fail(StructInvalidArity("set-struct!", 2, args.size)))
                else
                    val instance = args(0)
                    val newValue = args(1)
                    // join the new value with the old value
                    // TODO: check if this can be improved, if not integrated with strong updates
                    lat.getStructs(instance).map { struct =>
                        struct.fields(getterSetter.idx) = lat.join(struct.fields(getterSetter.idx), newValue)
                        monad.unit(lat.nil)
                    }
            else if args.size != 1 then Set(monad.fail(StructInvalidArity("struct-ref", 1, args.size)))
            else
                // Getter (posn-x inst)
                val instance = args(0)
                lat.getStructs(instance).map { struct =>
                  monad.unit(struct.fields(getterSetter.idx))
                }
          }
        val constr: Set[M[Value]] =
          getConstructor(op).map { case (tag, size) =>
            monad.unit(lat.struct(Struct(tag, Array.fill(size)(lat.nil))))
          }

        monad.mjoin(gettersSetters ++ constr)

object StructOps:
    def unapply(that: Any): Option[MakeStruct] =
      that match
          case ms: MakeStruct => Some(ms)
          case _              => None

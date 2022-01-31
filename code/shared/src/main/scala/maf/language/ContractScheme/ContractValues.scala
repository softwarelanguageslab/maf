package maf.language.ContractScheme

/** Soft-contract verification concrete internal values */

import maf.core.Identity
import maf.language.scheme._
import scala.reflect.ClassTag

object ContractValues:
    sealed trait Value[+L]

    /**
     * A blame represents two (possibly distinct) source locations.
     *
     * The `blamedPosition` represents the location of the code that violated a particular contract, while `blamingPosition` represents the location
     * of the contract that is being violated.
     */
    case class Blame(blamedPosition: Identity, blamingPosition: Identity) extends Value[Nothing]

    /**
     * A guard which represents the value of a dependent contract after evaluation. <code> (~> domain rangeMaker) </code>
     *
     * @tparam L
     *   the type of abstract value contained within the contract value
     */
    case class Grd[L](domain: List[L], rangeMaker: L, domainIdns: List[Identity], rangeMakerExpr: SchemeExp) extends Value[L]:
        def map[AL](f: L => AL): Grd[AL] = Grd(domain.map(f), f(rangeMaker), domainIdns, rangeMakerExpr)

    /**
     * A monitor on a dependent contract (mon (~> domain rangeMaker)/lcontract procedure/lserver)
     *
     * @tparam L
     *   the type of abstract value contained within the contract value
     */
    case class Arr[L](
        lcontract: Identity,
        lserver: Identity,
        contract: Grd[L],
        e: L,
        topLevel: Boolean = false)
        extends Value[L]:
        def map[AL](f: L => AL): Arr[AL] = Arr(lcontract, lserver, contract.map(f), f(e), topLevel)
        def checkArgs[A](l: List[A]): Boolean =
          contract.domain.size == l.size
        def expectedNumArgs: Int = contract.domain.size

    /**
     * A value that represents a flat contract, such that we can distribute blames correctly when a value of this type is applied.
     *
     * @param contract
     *   the wrapped contract
     * @param fexp
     *   the original expression AST node from (flat expr)
     * @param contractIdn
     *   the location in the source code where the flat contract orginated from
     * @param sym
     *   the symbolic representation of the flat contract (at definition time) if available
     * @tparam L
     *   the type of abstract value contained within the contract value
     */
    case class Flat[L](contract: L, fexp: SchemeExp, sym: Option[SchemeExp], contractIdn: Identity) extends Value[L]:
        def map[AL](f: L => AL): Flat[AL] = Flat(f(contract), fexp, sym, contractIdn)

    /**
     * A representation for an apaque value, opaque values are used as substitutes for any value and have the same semantics as a "top" value in a
     * lattice.
     *
     * The value is used instead of top because (1) it makes it explicit that it is coming from SCV and is not a result of some over-approximation (2)
     * allows for a "top" value to exist in any abstract domain, which is not possible in for example a (non-bounded) constant propagation lattice
     */
    case class Opq() extends Value[Nothing]

    /**
     * A struct value.
     *
     * A struct consists of a set of fields. The names of the fields are not kept as they can be compiled in the accessors and mutators of that
     * struct.
     *
     * For convience and debugging purposes the name of the struct is kept.
     *
     * A primitive called (_make-struct symbol number) is provided to create an instance of this struct. The primitive (_struct_ref instance number)
     * can be used to access a particular field, while (_struct_set! instance number value) can be used to set a field in the struct.
     */
    case class Struct[L](tag: String, fields: maf.util.ArrayEq[L]) extends Value[L]:
        def map[AL: ClassTag](f: L => AL): Struct[AL] =
          this.copy(fields = fields.map(f))

    /**
     * A struct getter/setter. Works just like an application of _struct_set!.
     *
     * It is provided as an additional value in order to achieve exact precision without requiring n-m-cfa with m >= 1.
     * @param tag
     *   the name of the struct, used for tagging purposes
     * @param idx
     *   the index of the field to receive/update
     * @param isSetter
     *   true if this value is a setter.
     */
    case class StructSetterGetter(tag: String, idx: Int, isSetter: Boolean) extends Value[Nothing]

    /**
     * A constructor for a struct
     *
     * @param tag
     *   the name of the struct used for tagging purposes
     * @param siz
     *   the number of fields in the struct
     */
    case class StructConstructor(tag: String, size: Int) extends Value[Nothing]

    /**
     * A predicate for a struct
     *
     * @param tag
     *   the name of the struct to use when checking the predicate in the semantics
     */
    case class StructPredicate(tag: String) extends Value[Nothing]

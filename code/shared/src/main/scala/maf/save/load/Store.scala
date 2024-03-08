package maf.save

import maf.core.Expression
import maf.modular.AbstractDomain
import maf.modular.GlobalStore
import io.bullet.borer.Decoder
import maf.modular.scheme.ModularSchemeDomain
import maf.language.scheme.SchemeExp
import maf.lattice.HMapKey
import maf.lattice.HMap
import io.bullet.borer.Reader
import maf.save.EncapsulatedDecoder.*
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.Concrete
import maf.lattice.ConstantPropagation
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.SchemeLambdaExp
import maf.core.Address
import maf.modular.scheme.modf.BaseSchemeModFSemanticsM

/**
 * The base trait for decoding [[AbstractDomain.Value values]].
 *
 * @note
 *   This trait gives the methods needed to decode values, but does not implement them yet, other traits like [[LoaNoContext]] should be mixed in for
 *   the implementation. The trait that should be mixed in depends on the kind of values that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadValue[Expr <: Expression] extends Load[Expr] with AbstractDomain[Expr]:
    /**
     * Get the decoder that will be used to decode values.
     *
     * This will influence how values will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]] decoder.
     */
    def getValueDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode values.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how values will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]] decoder.
     */
    def getValueKeyDecoder: AbstractDecoder = getKeyDecoder
    given valueDecoder: Decoder[Value]

/* Trait to decode lattices.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadLattice[Expr <: Expression] extends Load[Expr]:
    /**
     * The types of lattices that can be decoded by this trait.
     *
     * This is used to specify the givens, if this was not used, this given could be used for every class with a single abstract type.
     */
    type Lattice[T] = ConstantPropagation.L[T] | Concrete.L[T]

    /**
     * Get the decoder that will be used to decode lattices.
     *
     * This will influence how lattices will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getLatticeDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode lattices.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how lattices will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getLatticeKeyDecoder: AbstractDecoder = getKeyDecoder

    /** Returns a map that links a key to a specific decoder. */
    def latticeDecoders[T: Decoder] = Set[(String, Decoder[_ <: Lattice[T]])](("constant", constantLatticeDecoder[T]))

    given latticeDecoder[P[T] <: Lattice[T], T: Decoder]: EncapsulatedDecoder[P[T]] with
        override def decoder: AbstractDecoder = getLatticeKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using d: AbstractDecoder): P[T] =
            reader.readMembers[P[T]](latticeDecoders.toArray.asInstanceOf[Array[(String, io.bullet.borer.Decoder[? <: P[T]])]]).value

    given constantLatticeDecoder[T: Decoder]: Decoder[ConstantPropagation.L[T]] with
        override def read(reader: Reader): ConstantPropagation.L[T] =
            if reader.tryReadString("top") then return ConstantPropagation.Top
            else if reader.tryReadString("bottom") then ConstantPropagation.Bottom
            else return new ConstantPropagation.Constant[T](reader.read[T]())

/**
 * Trait to decode [[ModularSchemeLattice modular scheme lattices]].
 *
 * Implementation of [[LoadModularDomain]].
 */
trait LoadModularSchemeLattices
    extends LoadModularDomain
    with LoadAddr[SchemeExp]
    with LoadExpressions[SchemeExp]
    with BaseSchemeModFSemanticsM
    with LoadEnvironment[SchemeExp]
    with LoadLattice[SchemeExp]:
    type SchemeLattice = ModularSchemeLattice[?, ?, ?, ?, ?, ?, ?]
    override def hMapDecoders = super.hMapDecoders ++ Set(
      ("int", summon[Decoder[(HMapKey, SchemeLattice#Int)]]),
      ("boolean", summon[Decoder[(HMapKey, SchemeLattice#Bool)]]),
      ("string", summon[Decoder[(HMapKey, SchemeLattice#Str)]]),
      ("primitive", summon[Decoder[(HMapKey, SchemeLattice#Prim)]]),
      ("closure", summon[Decoder[(HMapKey, SchemeLattice#Clo)]]),
      ("pointer", summon[Decoder[(HMapKey, SchemeLattice#Pointer)]]),
      ("symbol", summon[Decoder[(HMapKey, SchemeLattice#Symbol)]]),
      ("cons", summon[Decoder[(HMapKey, SchemeLattice#Cons)]]),
      ("nil", summon[Decoder[(HMapKey, modularLattice.Nil.type)]]),
      ("void", summon[Decoder[(HMapKey, modularLattice.Void.type)]]),
    )

    given Decoder[(HMapKey, SchemeLattice#Int)] with
        override def read(reader: Reader): (HMapKey, SchemeLattice#Int) =
            val lattice = reader.read[Lattice[BigInt]]()
            return (modularLattice.IntT, new modularLattice.Int(lattice.asInstanceOf[LoadModularSchemeLattices.this.modularLatticeWrapper.I]))

    given Decoder[(HMapKey, SchemeLattice#Bool)] with
        override def read(reader: Reader): (HMapKey, SchemeLattice#Bool) =
            val lattice = reader.read[Lattice[Boolean]]()
            return (modularLattice.BoolT, new modularLattice.Bool(lattice.asInstanceOf[LoadModularSchemeLattices.this.modularLatticeWrapper.B]))

    given Decoder[(HMapKey, SchemeLattice#Str)] with
        override def read(reader: Reader): (HMapKey, SchemeLattice#Str) =
            val lattice = reader.read[Lattice[String]]()
            return (modularLattice.StrT, new modularLattice.Str(lattice.asInstanceOf[LoadModularSchemeLattices.this.modularLatticeWrapper.S]))

    given Decoder[(HMapKey, SchemeLattice#Symbol)] with
        override def read(reader: Reader): (HMapKey, SchemeLattice#Symbol) =
            val lattice = reader.read[Lattice[String]]()
            return (modularLattice.SymbolT, new modularLattice.Symbol(lattice.asInstanceOf[LoadModularSchemeLattices.this.modularLatticeWrapper.Sym]))

    given Decoder[(HMapKey, SchemeLattice#Prim)] with
        override def read(reader: Reader): (HMapKey, SchemeLattice#Prim) =
            return (modularLattice.PrimT, new modularLattice.Prim(reader.read[Set[String]]()))

    given EncapsulatedDecoder[(SchemeLambdaExp, Env)] with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (SchemeLambdaExp, Env) =
            val expression = reader.readMember[SchemeExp]("expression").asInstanceOf[ReadValue[String, SchemeLambdaExp]]
            val address = reader.readMember[Env]("address")
            return (expression.value, address.value)

    given EncapsulatedArrayDecoder[(HMapKey, SchemeLattice#Clo)]() with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, SchemeLattice#Clo) =
            return (modularLattice.CloT,
                    new modularLattice.Clo(
                      reader.readUntilBeforeBreak[Set[(SchemeLambdaExp, Env)]](Set[(SchemeLambdaExp, Env)](),
                                                                               (closures) => closures + (reader.read[(SchemeLambdaExp, Env)]())
                      )
                    )
            )

    given EncapsulatedArrayDecoder[(HMapKey, SchemeLattice#Pointer)]() with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, SchemeLattice#Pointer) =
            return (modularLattice.PointerT,
                    new modularLattice.Pointer(reader.readUntilBeforeBreak[Set[Address]](Set(), (pointers) => pointers + (reader.read[Address]())))
            )

    given EncapsulatedDecoder[(HMapKey, SchemeLattice#Cons)]() with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, SchemeLattice#Cons) =
            val car = reader.readMember[SchemeLattice#L]("car")
            val cdr = reader.readMember[SchemeLattice#L]("cdr")
            return (modularLattice.ConsT, new modularLattice.Cons(car.value, cdr.value))

    given Decoder[(HMapKey, modularLattice.Nil.type)] with
        override def read(reader: Reader): (HMapKey, modularLattice.Nil.type) = return (modularLattice.NilT, modularLattice.Nil)

    given Decoder[(HMapKey, modularLattice.Void.type)] with
        override def read(reader: Reader): (HMapKey, modularLattice.Void.type) = return (modularLattice.VoidT, modularLattice.Void)

/**
 * Base trait for decoding values as [[ModularSchemeLattice modular scheme lattices]], as defined in [[ModularSchemeDomain]].
 *
 * @note
 *   This trait gives the methods needed to decode values, but does not implement them yet, other traits like [[LoaNoContext]] should be mixed in for
 *   the implementation. The trait that should be mixed in depends on the kind of values that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadModularDomain extends LoadValue[SchemeExp] with ModularSchemeDomain:
    /**
     * Get the decoder that will be used to decode an hMap.
     *
     * This will influence how an hMap will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getHMapDecoder: AbstractDecoder = new ArrayDecoder

    /** Returns a map that links a key to a specific decoder. */
    def hMapDecoders = Set[(String, Decoder[_ <: (HMapKey, Any)])]()

    given EncapsulatedDecoder[(HMapKey, Any)] with
        override def decoder: AbstractDecoder = getValueKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, Any) =
            reader.readMembers(hMapDecoders.toArray).value

    override given valueDecoder: EncapsulatedDecoder[HMap] with
        override def decoder: AbstractDecoder = getHMapDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): HMap =
            return new HMap(reader.readUntilBeforeBreak[Map[HMapKey, Any]](Map(), (hMap) => hMap + reader.readMember[(HMapKey, Any)]().value))

/**
 * Trait to decode the global store.
 *
 * This adds the global store to the objects that should be loaded, but does not have an implementation that can be used to decode the
 * [[AbstractDomain.Value values]] inside of the store, for this an implementation of [[LoadValue]] like [[LoadModularDomain]] should be included
 * depending on the values that are used in your analysis.
 */
trait LoadGlobalStore[Expr <: Expression] extends LoadValue[Expr] with LoadAddr[Expr] with LoadMapToArray with GlobalStore[Expr]:
    override def loadInfo = super.loadInfo ++ List(("store", Loadable((store: Map[Addr, Value]) => this.store = store)))

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
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.Concrete
import maf.lattice.ConstantPropagation
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.SchemeLambdaExp
import maf.core.Address
import maf.modular.scheme.modf.BaseSchemeModFSemanticsM
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.lattice.ConstantPropagation.L

/**
 * The base trait for decoding [[AbstractDomain.Value values]].
 *
 * @note
 *   This trait gives the methods needed to decode values, but does not implement them yet, other traits like [[LoadModularDomain]] should be mixed in
 *   for the implementation. The trait that should be mixed in depends on the kind of values that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadValue[Expr <: Expression] extends Load[Expr] with AbstractDomain[Expr]:
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

    /** Returns a map that links a key to a specific decoder. */
    def latticeDecoders[T: Decoder] = Set[(String, Decoder[_ <: Lattice[T]])](("constant", constantLatticeDecoder[T]))

    given latticeDecoder[P[T] <: Lattice[T], T: Decoder]: MapDecoder[P[T]] with
        override def read(reader: Reader): P[T] =
            reader.start()
            val lattice = reader.readMembers[P[T]](latticeDecoders.toArray.asInstanceOf[Array[(String, io.bullet.borer.Decoder[? <: P[T]])]])
            reader.close()
            return lattice.value

    given constantLatticeDecoder[T: Decoder]: Decoder[ConstantPropagation.L[T]] with
        override def read(reader: Reader): ConstantPropagation.L[T] =
            if reader.tryReadString("top") then return ConstantPropagation.Top
            else if reader.tryReadString("bottom") then ConstantPropagation.Bottom
            else return new ConstantPropagation.Constant[T](reader.read[T]())

trait LoadModularSchemeDomainLattices extends Load[SchemeExp] with ModularSchemeDomain:
    given stringLatticeDecoder: Decoder[S]
    given booleanLatticeDecoder: Decoder[B]
    given integerLatticeDecoder: Decoder[I]
    given realLatticeDecoder: Decoder[R]
    given charLatticeDecoder: Decoder[C]
    given symbolLatticeDecoder: Decoder[Sym]

trait LoadSchemeConstantPropagationDomain extends SchemeConstantPropagationDomain with LoadModularSchemeDomainLattices with LoadLattice[SchemeExp]:
    override given stringLatticeDecoder: Decoder[S] = latticeDecoder[L, String].asInstanceOf[Decoder[S]]
    override given booleanLatticeDecoder: Decoder[B] = latticeDecoder[L, Boolean].asInstanceOf[Decoder[B]]
    override given integerLatticeDecoder: Decoder[I] = latticeDecoder[L, BigInt].asInstanceOf[Decoder[I]]
    override given realLatticeDecoder: Decoder[R] = latticeDecoder[L, Double].asInstanceOf[Decoder[R]]
    override given charLatticeDecoder: Decoder[C] = latticeDecoder[L, Char].asInstanceOf[Decoder[C]]
    override given symbolLatticeDecoder: Decoder[Sym] = latticeDecoder[L, String].asInstanceOf[Decoder[Sym]]

/**
 * Trait to decode [[ModularSchemeLattice modular scheme lattices]].
 *
 * Implementation of [[LoadModularDomain]].
 */
trait LoadModularSchemeDomain
    extends LoadModularDomain
    with LoadAddr[SchemeExp]
    with LoadExpressions[SchemeExp]
    with BaseSchemeModFSemanticsM
    with LoadEnvironment[SchemeExp]
    with LoadLattice[SchemeExp]
    with LoadComponents[SchemeExp]
    with LoadSchemeConstantPropagationDomain:
    type LoadSchemeLattice = ModularSchemeLattice[?, S, B, I, R, C, Sym]
    override def hMapDecoders = super.hMapDecoders ++ Set(
      ("int", summon[Decoder[(HMapKey, LoadSchemeLattice#Int)]]),
      ("boolean", summon[Decoder[(HMapKey, LoadSchemeLattice#Bool)]]),
      ("string", summon[Decoder[(HMapKey, LoadSchemeLattice#Str)]]),
      ("real", summon[Decoder[(HMapKey, LoadSchemeLattice#Real)]]),
      ("char", summon[Decoder[(HMapKey, LoadSchemeLattice#Char)]]),
      ("inputPort", summon[Decoder[(HMapKey, LoadSchemeLattice#InputPort)]]),
      ("kont", KDecoder),
      ("primitive", summon[Decoder[(HMapKey, LoadSchemeLattice#Prim)]]),
      ("closure", summon[Decoder[(HMapKey, LoadSchemeLattice#Clo)]]),
      ("pointer", summon[Decoder[(HMapKey, LoadSchemeLattice#Pointer)]]),
      ("symbol", summon[Decoder[(HMapKey, LoadSchemeLattice#Symbol)]]),
      ("cons", summon[Decoder[(HMapKey, LoadSchemeLattice#Cons)]]),
      ("vector", summon[Decoder[(HMapKey, LoadSchemeLattice#Vec)]]),
      ("nil", nilLatticeDecoder),
      ("void", summon[Decoder[(HMapKey, modularLattice.Void.type)]]),
      ("ERROR", errorDecoder)
    )

    private given errorDecoder: Decoder[(HMapKey, modularLattice.Nil.type)] with
        override def read(reader: Reader): (HMapKey, modularLattice.Nil.type) =
            val error = reader.readString()
            System.err.nn.println("The lattice was not correctly encoded and had error: `" + error + "`, using `nil` instead.")
            return (modularLattice.NilT, modularLattice.Nil)

    given Decoder[(HMapKey, LoadSchemeLattice#Int)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Int) =
            val lattice = reader.read[I]()
            return (modularLattice.IntT, new modularLattice.Int(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#Real)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Real) =
            val lattice = reader.read[R]()
            return (modularLattice.RealT, new modularLattice.Real(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#Bool)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Bool) =
            val lattice = reader.read[B]()
            return (modularLattice.BoolT, new modularLattice.Bool(lattice))

    private given KDecoder: Decoder[(HMapKey, LoadSchemeLattice#K)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#K) =
            val lattice = reader.read[Set[Component]]().asInstanceOf[Set[LoadSchemeLattice#K]]
            return (modularLattice.KontT, new modularLattice.Kont(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#Char)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Char) =
            val lattice = reader.read[C]()
            return (modularLattice.CharT, new modularLattice.Char(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#InputPort)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#InputPort) =
            val lattice = reader.read[LoadSchemeLattice#L]()
            return (modularLattice.InputPortT, new modularLattice.InputPort(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#Str)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Str) =
            val lattice = reader.read[S]()
            return (modularLattice.StrT, new modularLattice.Str(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#Symbol)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Symbol) =
            val lattice = reader.read[Sym]()
            return (modularLattice.SymbolT, new modularLattice.Symbol(lattice))

    given Decoder[(HMapKey, LoadSchemeLattice#Prim)] with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Prim) =
            return (modularLattice.PrimT, new modularLattice.Prim(reader.read[Set[String]]()))

    given MapDecoder[(SchemeLambdaExp, Env)] with
        override def read(reader: Reader): (SchemeLambdaExp, Env) =
            reader.start()
            val expression = reader.readMember[SchemeExp]("expression").asInstanceOf[ReadValue[String, SchemeLambdaExp]]
            val address = reader.readMember[Env]("address")
            reader.close()
            return (expression.value, address.value)

    given ArrayDecoder[(HMapKey, LoadSchemeLattice#Clo)]() with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Clo) =
            reader.start()
            val closures = reader.readUntilBeforeBreak[Set[(SchemeLambdaExp, Env)]](Set[(SchemeLambdaExp, Env)](),
                                                                                    (closures) => closures + (reader.read[(SchemeLambdaExp, Env)]())
            )
            reader.close()
            return (modularLattice.CloT, new modularLattice.Clo(closures))

    given ArrayDecoder[(HMapKey, LoadSchemeLattice#Pointer)]() with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Pointer) =
            reader.start()
            val pointers = reader.readUntilBeforeBreak[Set[Address]](Set(), (pointers) => pointers + (reader.read[Address]()))
            reader.close()
            return (modularLattice.PointerT, new modularLattice.Pointer(pointers))

    given MapDecoder[(HMapKey, LoadSchemeLattice#Cons)]() with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Cons) =
            reader.start()
            val car = reader.readMember[LoadSchemeLattice#L]("car")
            val cdr = reader.readMember[LoadSchemeLattice#L]("cdr")
            reader.close()
            return (modularLattice.ConsT, new modularLattice.Cons(car.value, cdr.value))

    given MapDecoder[(HMapKey, LoadSchemeLattice#Vec)] with LoadMapToArray with
        override def read(reader: Reader): (HMapKey, LoadSchemeLattice#Vec) =
            reader.start()
            val size = reader.readMember[I]("size").value
            val elements =
                reader
                    .readMember[Map[I, LoadSchemeLattice#L]]("elements")
                    .value
                    .asInstanceOf[Map[LoadModularSchemeDomain.this.modularLatticeWrapper.I, LoadSchemeLattice#L]]
            reader.close()
            return (modularLattice.VecT, new modularLattice.Vec(size, elements))

    given nilLatticeDecoder: Decoder[(HMapKey, modularLattice.Nil.type)] with
        override def read(reader: Reader): (HMapKey, modularLattice.Nil.type) =
            reader.readString()
            return (modularLattice.NilT, modularLattice.Nil)

    given Decoder[(HMapKey, modularLattice.Void.type)] with
        override def read(reader: Reader): (HMapKey, modularLattice.Void.type) =
            reader.readString()
            return (modularLattice.VoidT, modularLattice.Void)

/**
 * Base trait for decoding values as [[ModularSchemeLattice modular scheme lattices]], as defined in [[ModularSchemeDomain]].
 *
 * @note
 *   This trait gives the methods needed to decode values, but does not implement them yet, other traits like [[LoadModularSchemeDomain]] should be
 *   mixed in for the implementation. The trait that should be mixed in depends on the kind of values that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadModularDomain extends LoadValue[SchemeExp] with ModularSchemeDomain:
    /** Returns a map that links a key to a specific decoder. */
    def hMapDecoders = Set[(String, Decoder[_ <: (HMapKey, Any)])]()

    given MapDecoder[(HMapKey, Any)] with
        override def read(reader: Reader): (HMapKey, Any) =
            reader.start()
            val hmap = reader.readMembers(hMapDecoders.toArray)
            reader.close()
            return hmap.value

    override given valueDecoder: ArrayDecoder[HMap] with
        override def read(reader: Reader): HMap =
            reader.start()
            val hmap = reader.readUntilBeforeBreak[Map[HMapKey, Any]](Map(), (hMap) => hMap + reader.readMember[(HMapKey, Any)]().value)
            reader.close()
            return new HMap(hmap)

/**
 * Trait to decode the global store.
 *
 * This adds the global store to the objects that should be loaded, but does not have an implementation that can be used to decode the
 * [[AbstractDomain.Value values]] inside of the store, for this an implementation of [[LoadValue]] like [[LoadModularDomain]] should be included
 * depending on the values that are used in your analysis.
 */
trait LoadGlobalStore[Expr <: Expression] extends LoadValue[Expr] with LoadAddr[Expr] with LoadMapToArray with GlobalStore[Expr]:
    override def loadInfo = super.loadInfo ++ List(("store", Loadable((store: Map[Addr, Value]) => this.store = store)))

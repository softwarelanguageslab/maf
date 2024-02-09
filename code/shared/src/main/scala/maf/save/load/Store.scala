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

trait LoadValue[Expr <: Expression] extends Load[Expr] with AbstractDomain[Expr]:
    def getValueDecoder: AbstractDecoder = getDecoder
    def getValueKeyDecoder: AbstractDecoder = getKeyDecoder
    given valueDecoder: Decoder[Value]

trait LoadLattice[Expr <: Expression] extends Load[Expr]:
    type Lattice[T] = ConstantPropagation.L[T] | Concrete.L[T]

    def getLatticeDecoder: AbstractDecoder = getDecoder
    def getLatticeKeyDecoder: AbstractDecoder = getKeyDecoder
    def latticeDecoders[T: Decoder] = Set[(String, Decoder[_ <: Lattice[T]])](("constant", constantLatticeDecoder[T]))

    given latticeDecoder[P[T] <: Lattice[T], T: Decoder]: EncapsulatedDecoder[P[T]] with
        override def decoder: AbstractDecoder = getLatticeKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using d: AbstractDecoder): P[T] =
            reader.readMembers[P[T]](latticeDecoders.toArray.asInstanceOf[Array[(String, io.bullet.borer.Decoder[? <: P[T]])]]).value.get.get

    given constantLatticeDecoder[T: Decoder]: Decoder[ConstantPropagation.L[T]] with
        override def read(reader: Reader): ConstantPropagation.L[T] =
            if reader.tryReadString("top") then return ConstantPropagation.Top
            else if reader.tryReadString("bottom") then ConstantPropagation.Bottom
            else return new ConstantPropagation.Constant[T](reader.read[T]())

trait LoadModularSchemeLattices
    extends LoadModularDomain
    with LoadAddr[SchemeExp]
    with LoadStandardSchemeComponents
    with LoadEnvironment[SchemeExp]
    with LoadLattice[SchemeExp]:
    type SchemeLattice = ModularSchemeLattice[?, ?, ?, ?, ?, ?, ?]
    override def hMapDecoders = super.hMapDecoders + (
      ("int", summon[Decoder[(HMapKey, SchemeLattice#Int)]]),
      ("boolean", summon[Decoder[(HMapKey, SchemeLattice#Bool)]]),
      ("string", summon[Decoder[(HMapKey, SchemeLattice#Str)]]),
      ("primitive", summon[Decoder[(HMapKey, SchemeLattice#Prim)]]),
      ("closure", summon[Decoder[(HMapKey, SchemeLattice#Clo)]]),
      ("pointer", summon[Decoder[(HMapKey, SchemeLattice#Pointer)]])
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

    given Decoder[(HMapKey, SchemeLattice#Prim)] with
        override def read(reader: Reader): (HMapKey, SchemeLattice#Prim) =
            return (modularLattice.PrimT, new modularLattice.Prim(reader.read[Set[String]]()))

    given EncapsulatedDecoder[(SchemeLambdaExp, Env)] with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (SchemeLambdaExp, Env) =
            val expression = reader.readMember[SchemeLambdaExp]("expression")
            val address = reader.readMember[Env]("address")
            return (expression.value.get.get, address.value.get.get)

    given EncapsulatedArrayDecoder[(HMapKey, SchemeLattice#Clo)]() with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, SchemeLattice#Clo) =
            return (modularLattice.CloT,
                    new modularLattice.Clo(
                      reader.readUntilBeforeBreak[Set[(SchemeLambdaExp, Env)]](Set[(SchemeLambdaExp, Env)]())((closures) =>
                          closures + (reader.read[(SchemeLambdaExp, Env)]())
                      )
                    )
            )

    given EncapsulatedArrayDecoder[(HMapKey, SchemeLattice#Pointer)]() with
        override def decoder: AbstractDecoder = getValueDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, SchemeLattice#Pointer) =
            return (modularLattice.PointerT,
                    new modularLattice.Pointer(reader.readUntilBeforeBreak[Set[Address]](Set())((pointers) => pointers + (reader.read[Address]())))
            )

trait LoadModularDomain extends LoadValue[SchemeExp] with ModularSchemeDomain:
    def getHMapDecoder: AbstractDecoder = new ArrayDecoder
    def hMapDecoders = Set[(String, Decoder[_ <: (HMapKey, Any)])]()

    given EncapsulatedDecoder[(HMapKey, Any)] with
        override def decoder: AbstractDecoder = getValueKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): (HMapKey, Any) =
            reader.readMembers(hMapDecoders.toArray).value.get.get

    override given valueDecoder: EncapsulatedDecoder[HMap] with
        override def decoder: AbstractDecoder = getHMapDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): HMap =
            return new HMap(reader.readUntilBeforeBreak[Map[HMapKey, Any]](Map())((hMap) => hMap + reader.readMember[(HMapKey, Any)]().value.get.get))

trait LoadGlobalStore[Expr <: Expression] extends LoadValue[Expr] with LoadAddr[Expr] with LoadMapToArray with GlobalStore[Expr]:
    override def loadInfo: Map[String, Loadable[?]] = super.loadInfo + ("store" -> Loadable((store: Map[Addr, Value]) => println(store)))

package maf.modular.components

import maf.core._
import maf.modular.ModAnalysis

// A component pointer just is an integer.
case class ComponentPointer(addr: Int): //TODO: extends AnyVal doesn't work anymore (Scala bug?)
    override def toString: String = s"#$addr"

/** Provides the ability to reference components 'by pointer'. */
trait IndirectComponents[Expr <: Expression] extends ModAnalysis[Expr]:

    /** Every component is a pointer to an 'actual component'. */
    type Component = ComponentPointer
    def initialComponent = ref(initialComponentData)

    /** The 'actual component (data)' can be anything. */
    type ComponentData
    def initialComponentData: ComponentData

    // Keep a mapping from component pointer addresses to actual component data.
    type Address = Int
    private var count: Address = _ // Next free address.
    protected var cMap: Map[Address, ComponentData] = _
    protected var cMapR: Map[ComponentData, Address] = _

    /** Returns the next unused address. */
    protected def alloc(): Address =
        val addr = count
        count += 1
        addr

    /** Registers a component at a given address. */
    protected def register(cmp: ComponentData, addr: Address): Unit =
        cMap = cMap + (addr -> cmp)
        cMapR = cMapR + (cmp -> addr)

    /** Creates a component (pointer) from an 'actual component'. */
    private def newComponent(cmp: ComponentData): Address =
        val addr = alloc()
        register(cmp, addr)
        addr

    /** Returns the pointer corresponding to an (actual) component. */
    def ref(cmp: ComponentData): Component = ComponentPointer(cMapR.getOrElse(cmp, newComponent(cmp)))

    /** Retrieves the component data corresponding to a given component pointer. */
    def deref(ptr: ComponentPointer): ComponentData = cMap(ptr.addr)

    /** Allows to treat a component pointer as a component. */
    implicit def view(cmp: Component): ComponentData = deref(cmp)

    override def init() =
        count = 0
        cMap = Map()
        cMapR = Map()
        super.init()

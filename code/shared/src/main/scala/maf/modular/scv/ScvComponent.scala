package maf.modular.scv

import maf.util.SmartHash
import maf.modular.scheme.modf.SchemeModFComponent

/**
 * A soft-contract verification component.
 *
 * Components of this type usually wrap regular components, such as processes or functions and annotes them with additional information. Therefore,
 * there exists a trivial morphism from an ScvComponent to the component type of choice.
 *
 * @tparam T
 *   the type of the wrapped component
 */
sealed trait ScvComponent[T, L] extends SmartHash:
    def view: T

object ScvComponent:
    /** A simple wrapper which is returned by default when a component is created */
    case class SimpleWrapper[T, L](cmp: T) extends ScvComponent[T, L]:
        def view: T = cmp

    /**
     * A wrapper that indicates that the component needs to check whether the range conract is satisfied after the evaluation of the body of the
     * component
     */
    case class ContractCall[T, L](cmp: T, rangeMaker: L) extends ScvComponent[T, L]:
        def view: T = cmp

trait StandardScvModFComponents extends maf.modular.scv.ScvBaseSemantics:
    import ScvComponent.*
    import SchemeModFComponent.*
    type Component = ScvComponent[SchemeModFComponent, Value]

    def initialComponent: Component = SimpleWrapper(Main)

    def newComponent(call: Call[ComponentContext]): Component =
      SimpleWrapper(call)

    def newComponentWithContract(contract: Value)(cmp: Call[ComponentContext]): Component =
      ContractCall(cmp, contract)

    def view(cmp: Component): SchemeModFComponent = cmp.view

    def usingContract[X](cmp: Component)(f: Option[Value] => X): X = cmp match
        case ContractCall(_, contract) => f(Some(contract))
        case _                         => f(None)

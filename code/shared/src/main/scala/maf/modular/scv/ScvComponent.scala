package maf.modular.scv

//import maf.util.SmartHash
//import maf.modular.scheme.modf.SchemeModFComponent
//import maf.core.Identity
//import maf.language.scheme.SchemeExp
//
///**
// * A soft-contract verification component.
// *
// * Components of this type usually wrap regular components, such as processes or functions and annotes them with additional information. Therefore,
// * there exists a trivial morphism from an ScvComponent to the component type of choice.
// *
// * @tparam T
// *   the type of the wrapped component
// */
//sealed trait ScvComponent[T, L] extends SmartHash:
//    def view: T
//
//object ScvComponent:
//    /** A simple wrapper which is returned by default when a component is created */
//    case class SimpleWrapper[T, L](cmp: T) extends ScvComponent[T, L]:
//        def view: T = cmp
//
//    /**
//     * A wrapper that indicates that the component needs to check whether the range conract is satisfied after the evaluation of the body of the
//     * component
//     *
//     * @param cmp
//     *   the original component that is wrapped with additional information
//     * @param domains
//     *   a list of domain contracts that where checked before calling the function
//     * @param rangeContract
//     *   the range contract which can be used to check whether the result value of a component satisfies a particular contract
//     * @param args
//     *   the syntactic arguments used when calling the function
//     * @param idn
//     *   the identity associated with the original function call
//     */
//    case class ContractCall[T, L](cmp: T, domains: List[L], rangeContract: L, args: List[SchemeExp], idn: Identity) extends ScvComponent[T, L]:
//        def view: T = cmp
//
//trait StandardScvModFComponents extends maf.modular.scv.ScvBaseSemantics:
//    import ScvComponent.*
//    import SchemeModFComponent.*
//    type Component = ScvComponent[SchemeModFComponent, Value]
//
//    def initialComponent: Component = SimpleWrapper(Main)
//
//    def newComponent(call: Call[ComponentContext]): Component =
//      SimpleWrapper(call)
//
//    def newComponentWithContract(
//        contract: Value,
//        domains: List[Value],
//        args: List[SchemeExp],
//        idn: Identity
//      )(
//        cmp: Call[ComponentContext]
//      ): Component =
//      ContractCall(cmp, domains, contract, args, idn)
//
//    def view(cmp: Component): SchemeModFComponent = cmp.view
//
//    def usingContract[X](cmp: Component)(f: Option[(List[Value], Value, List[SchemeExp], Identity)] => X): X = cmp match
//        case ContractCall(_, domains, contract, args, idn) => f(Some(domains, contract, args, idn))
//        case _                                             => f(None)

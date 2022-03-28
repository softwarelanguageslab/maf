package maf.language.sexp

import maf.core.Identity

object SExpUtils:
    object :::: :
        def unapply(value: SExp): Option[(SExp, SExp)] =
            value match
                case SExpPair(car, cdr, _) => Some((car, cdr))
                case _                     => None

    object Ident:
        def unapply(value: SExpId): Option[(String)] = Some(value.id.name)

    object IdentWithIdentity:
        def unapply(arg: SExpId): Option[(String, Identity)] =
            Some((arg.id.name, arg.idn))

    object SNil:
        def unapply(value: SExp): Option[(Identity)] = value match
            case SExpValue(Value.Nil, idn) => Some(idn)
            case _                         => None

    /** Maps over a list of s-expressions */
    def smap[A](lst: SExp, f: (SExp) => A): List[A] = lst match
        case SExpPair(car, cdr, _) =>
            f(car) :: smap(cdr, f)
        case snil => List()

    /**
     * Converts the given s-expression to a Scala list.
     *
     * If the given s-expression is not structued as a list (a sequence of pairs ending with the empty list) then an exception is thrown.
     *
     * @param sexp
     *   the s-expression to convert to a list
     * @return
     *   a Scala list of s-expressions
     */
    extension (sexp: SExp)
        def toList: List[SExp] = sexp match
            case SNil(_)      => List()
            case car :::: cdr => car :: cdr.toList
            case _            => throw new Exception("not a list")

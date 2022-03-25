package maf.modular.scv

sealed trait Count
case object Zero extends Count
case object Once extends Count
case object MoreThanOnce extends Count

/**
 * This trait keeps track of which addresses have been updated during the analysis.
 *
 * An address is updated if it has been at least twice written to (with a different value) in the abstract analysis
 */
trait ScvUpdateCount extends ScvModAnalysis:
    private var writeCounts: Map[Addr, Count] = Map().withDefaultValue(Zero)

    override def writeAddr(addr: Addr, value: Value): Boolean =
        if super.writeAddr(addr, value) then
            writeCounts = writeCounts(addr) match
                case Zero         => writeCounts + (addr -> Once)
                case Once         => writeCounts + (addr -> MoreThanOnce)
                case MoreThanOnce => writeCounts
            true
        else false

    def isUpdated(addr: Addr): Boolean =
        writeCounts(addr) match
            case MoreThanOnce => true
            case _            => false

package maf.modular.scheme.aam

/**

import maf.core.*
import maf.modular.*
import maf.modular.scheme.*
import maf.language.scheme.*
import maf.util.Wrapper
import maf.core.Store.*
import maf.util.benchmarks.Timeout.T

trait AAMStoreWidening extends AAMScheme:
    outer: SchemeDomain with AAMSchemeSensitivity =>

    // keep track of the component currently being analysed
    // TODO: this is ugly!
    var currentAnalysis: Option[AAMGlobalStoreIntra] = None
    override def intraAnalysis(ς: State) = new IntraAnalysis(ς) with AAMGlobalStoreIntra
    trait AAMGlobalStoreIntra extends IntraAnalysis:
        val globalStore = outer.globalStore
        val globalKStore = outer.globalKStore
        override def analyzeWithTimeout(timeout: T) =
            currentAnalysis = Some(this)
            super.analyzeWithTimeout(timeout)
        override def doWrite(dep: Dependency) = dep match
            case AddrDep(a) => ???
            case KAddrDep(aₖ) => ???
            case _ => super.doWrite(dep)

    // value store

    given sto: Store[SimpleStore[Adr, Val]] = Store.simpleInstance
    var globalStore = sto.empty
    case class AddrDep(a: Adr) extends Dependency
    private given globalStoreInstance: Store[Unit] with
        def empty = ()
        extension (ignore: Unit)
            def lookup(a: Adr) = 
                currentAnalysis.foreach(_.register(AddrDep(a)))
                sto(currentAnalysis.globalStore)(a)
            def extend(a: Adr, v: Val) = 
                val prv = globalStore
                globalStore = globalStore.extend(a, v)
                if sto(globalStore)(a) != sto(prv)(a) then
                    currentAnalysis.foreach(_.trigger(AddrDep(a)))

    override def storeWrapper = Wrapper.wrapper(using globalStoreInstance: Store[Unit])
    
    // kont store

    given ksto: KStore[SimpleStore[KAdr, Kon]] = Store.simpleInstance
    var globalKStore = ksto.empty
    case class KAddrDep(aₖ: KAdr) extends Dependency
    private given globalKStoreInstance: KStore[Unit] with
        def empty = ()
        extension (ignore: Unit)
            def lookup(aₖ: KAdr) = 
                currentAnalysis.foreach(_.register(KAddrDep(aₖ)))
                ksto(globalKStore)(aₖ)
            def extend(aₖ: KAdr, κ: Kon) = 
                val prv = globalKStore
                globalKStore = globalKStore.extend(aₖ, κ)
                if ksto(globalKStore)(aₖ) != ksto(prv)(aₖ) then
                    currentAnalysis.foreach(_.trigger(KAddrDep(aₖ)))

    override def kstoreWrapper = Wrapper.wrapper(using globalKStoreInstance: KStore[Unit])

    */
package maf.modular.scheme.aam

import maf.modular.scheme.*
import maf.util.Wrapper
import maf.core.Store
import maf.core.Store.*

trait AAMAbstractCounting extends AAMScheme:
    this: SchemeDomain with AAMSchemeSensitivity => 

    // only count for pointer addresses, as only they are mutable!
    given shouldCount: (Adr => Boolean) =
        case _: PAdr => true    
        case _       => false
            
    override def storeWrapper = Wrapper.wrapper(using Store.countingInstance: Store[CountingStore[Adr, Val]])
package maf.aam.scheme

import maf.core.*
import scala.annotation.static

/**
 * An imperative global store, as described in "Optimizing Abstract Abstract Machine" section 5.5 "Imperative, pre-allocated datastructures".
 *
 * To preserve memory, we do not pre-allocate the datastructures, by using a hash map to dynamically add new keys when required. This has a potential
 * impact on the peformance, since these maps could contain collisions, reverting in the worst case to a linear search for the key that has
 * collisions. However, we argue that the probability of having a hash collision is sufficiently low to negate any potential performance impacts
 * compared to the memory impact.
 *
 * In the transition relation, we use a logging store, so that changes are logged and applied after the state has finished processing completely. The
 * transition relation itself never uses any of the updates it makes to the store. Therefore changes can be replayed in the global store after the
 * state has been fully processed.
 *
 * Additionally, each update increments a timestamp variable, and a history is kept of values at previous timestamps. This is to make sure that
 * changes cannot travel back to the past, and are only propagated to the future.
 *
 * TODO: check cross-branch contamination
 */
trait SchemeImperativeStoreWidening extends BaseSchemeAAMSemantics:
    class ValStack(contents: List[(Int, Storable)])
    class ImperativeStore(contents: Map[Address, ValStack] = Map())

    protected var store: ImperativeStore = ImperativeStore()

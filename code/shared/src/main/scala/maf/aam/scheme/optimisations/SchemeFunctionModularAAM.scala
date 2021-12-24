package maf.aam.scheme.optimisations

import maf.aam.scheme.stores.SchemeImperativeStoreWidening
import maf.language.scheme.SchemeExp
import maf.aam.scheme.SchemeStoreAllocateReturn
import maf.core.Monad.MonadSyntaxOps

/**
 * Behaves like ModF in the sense that it uses an effect driven global store and analyses the continuation of a function directly instead of analysing
 * the function body first
 */
trait SchemeFunctionModularAAM extends SchemeImperativeStoreWidening, SchemeStoreAllocateReturn:
    // TODO: wrong, the rest of the function cannot be evaluated in the environment of the successor!
    override def call(
        env: Env,
        sto: Sto,
        kon: KonA,
        body: List[SchemeExp],
        t: Timestamp,
        ext: Ext
      ): Result =
        println(s"call of $body")
        for {
          // instead of directly returning the config we store it for later analysis
          callConf <- evaluate_sequence(env, sto, kon, body, t, ext, true)
          (v, sto1): (Val, Sto) = readStoV(sto, super[SchemeStoreAllocateReturn].allocRet(kon), ext)
          // also register the call effect
          sto2 = sto1.callDep(callConf)
          // continue with the continuation of the function, using an (old) value from the global store
          _ = { println(s"current continuation =  $kon") }
          result <- continue(v, sto2, kon, t, ext)
          _ = { println(s"got result $result") }
        } yield result

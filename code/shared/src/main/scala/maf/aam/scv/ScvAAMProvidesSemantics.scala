package maf.aam.scv

import maf.language.scheme.*
import maf.util.Trampoline.*
import maf.core.Monad.*
import maf.core.{Identifier, Identity}
import maf.language.ContractScheme.ContractValues.*

/**
 * Adds support for the `provide` expression in AAM style. Similar to the paper "Soft contract Verification for higher-order stateful programs", we
 * execute any function provided using `contract-out` with opaque values in order to kickstart the analysis.
 *
 * @see
 *   maf.modular.scv.ScvBigStepWithProvides
 */
trait ScvAAMProvidesSemantics extends ScvAAMSemantics:
    /** The continuation of evaluating all the contracts in the `contract-out` expression */
    case class EvalGuardsFrame(
        values: List[(Val, Identifier)],
        collectedContracts: List[Val],
        remainingContracts: List[SchemeExp],
        contractOutIdentities: List[Identity],
        env: Env,
        next: Option[KonA] = None)
        extends Frame:
        def link(next: KonA): EvalGuardsFrame = this.copy(next = Some(next))

    /**
     * Read the value from the identifiers in the contract-out expression
     *
     * @return
     *   the identifier and the value of that identifier
     */
    private def readValue(out: ContractSchemeProvideOut, env: Env, sto: Sto, ext: Ext): (Val, Identifier) =
      out match
          case ContractSchemeContractOut(name, _, _) =>
            // we ignore the resulting store (this ignores any read effects if a global effect store is used)
            (env.lookup(name.name).map(readStoV(sto, _, ext)._1).getOrElse(throw new Exception("provided identifier not found")), name)
          case _ => throw new Exception("only contract-out is supported")

    /**
     * Wrap the given value with a contract, if the value is not a function (or the contract not a grd) we simply ignore it
     *
     * @return
     *   a set of values of potential guarded functions
     */
    private def wrapValue(vlu: Val, contract: Val, expIdn: Identity, monIdn: Identity): Set[Arr[LatVal]] =
      lattice.getGrds(project(contract)).map { grd =>
          val arr = Arr(monIdn, expIdn, grd, project(vlu))
          arr
      }

    private def fresh(idn: Identity): SchemeExp = SchemeFuncall(SchemeVar(Identifier("fresh", idn)), List(), idn)

    override def eval(
        exp: SchemeExp,
        env: Env,
        sto: Sto,
        kont: KonA,
        t: Timestamp,
        ext: Ext
      ): Result = exp match
        // the process is slightly different from ModF, first we will wrap all the functions into monited
        // functions with the given contract. Then we will update the store such  that the arguments of all
        // the function calls are already written. Then finally, we generate seperate successor
        // states for all of the functions.
        //
        // TODO: check how the Nguyen analysis takes into account that the provided functions may be
        // executed in an arbitrary combination and order at execution, and that the analysis should
        // be able to take this into account. In effect-driven global stores we get this basically for
        // free as all the states are triggered in this location, and any read dependencies will be registered
        // in the future, such that if another state writes to it, the order of analysis does not really matter.
        case ContractSchemeProvide(outs, _) =>
          // a list of values corresponding to the identifiers given in the provides
          val values = outs.map(readValue(_, env, sto, ext))

          // evaluate the contracts
          val contracts = outs.collect { case ContractSchemeContractOut(_, contract, _) =>
            (contract, contract.idn)
          }

          if contracts.isEmpty then
              // nothing to do here, just continue with the continuation using `ap`
              ap(inject(lattice.nil), sto, kont, t, ext)
          else
              val contractsExp = contracts.map(_._1)
              val contractsMonIdns = contracts.map(_._2)

              // evaluate the current contract, and after that the other contracts
              val next = EvalGuardsFrame(values, List(), contractsExp.tail, contractsMonIdns, env)
              pushFrameEv(contractsExp.head, env, sto, kont, next, t, ext)

        case _ => super.eval(exp, env, sto, kont, t, ext)

    override def continue(vlu: Val, sto: Sto, kon: KonA, t: Timestamp, ext: Ext): Result =
        import maf.util.CollectionUtils.*
        readKonts(sto, kon).map { (kont, sto) =>
          kont match
              case EvalGuardsFrame(values, collectedContracts, List(), monIdns, env, Some(next)) =>
                // now we can wrap the functions into their own contracts
                // and apply them with opaque values
                val wrappedValues = values.zip2(collectedContracts, monIdns).map { case ((vlu, name), contract, contractIdn) =>
                  wrapValue(vlu, contract, name.idn, contractIdn)
                }
                // then apply the wrapped values
                wrappedValues
                  .zip(values.map(_._2))
                  .flatMap { case (vlu, name) =>
                    vlu.map { vlu =>
                      applyFun(
                        /** Synthetic syntactic representation of the guarded function call */
                        SchemeFuncall(SchemeVar(name), (0 to vlu.expectedNumArgs).map(_ => fresh(name.idn)).toList, Identity.none),
                        /* Inject the value in the domain */
                        inject(lattice.arr(vlu)),
                        /** Call the guarded function with opaque values */
                        (0 to vlu.expectedNumArgs).map(_ => inject(lattice.opq(Opq()))).toList,
                        /* Regular AAM information */
                        env,
                        sto,
                        kon,
                        t,
                        ext
                      )
                    }
                  }
                  .flattenM

              case f @ EvalGuardsFrame(values, collectedContracts, contract :: remaining, monIdns, env, Some(next)) =>
                pushFrameEv(contract, env, sto, next, f.copy(collectedContracts = vlu :: collectedContracts, remaining, monIdns, env), t, ext)

              case _ => super.continue(vlu, sto, kon, t, ext)
        }.flattenM

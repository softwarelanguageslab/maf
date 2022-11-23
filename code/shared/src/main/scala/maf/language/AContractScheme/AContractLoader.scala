package maf.language.AContractScheme

case class SchemeProject()

/**
 * Load a file that interacts with the acontracts library.
 *
 * For this the `require` special forms present in these programs need to be resolved, and their required identifiers required.
 *
 * One exception of this rule is the inclusion of the acontracts/actor library which we do not aim to support. Rather, our static analysis provides
 * its own semantics for actors that corresponds to the actor semantics in the contract library.
 *
 * To have mirror support in this actor language, the semantics implemented in modactors.mirrors can be used. It provides a meta protocol that matches
 * the one found in the contract library.
 */
object AContractLoader:
    def loadProgram(programName: String): SchemeProject = ???

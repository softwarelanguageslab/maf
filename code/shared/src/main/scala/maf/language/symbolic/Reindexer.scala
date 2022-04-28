package maf.language.symbolic

import maf.core.{Address, Identifier, Identity, Position}
import Symbolic.*
import maf.util.CollectionUtils

/** This object provides functions to reindex (or alpha-rename) parts of the components in an Scv analysis. */
object Reindexer:
    private type SymStore = Map[Address, Symbolic]
    type Delta = List[SymChange]

    /** Compute a delta where all of the variables in the given parts are tagged with the given tag */
    //def tag(pc: PathCondition, m: SymStore, d: Delta, tag: Position.PTag, except: Set[Var] = Set()): Delta =
    //    // we get all the variables
    //    val variables = (pc.varsExp ++ SymbolicStore.variableExpressions(m) ++ d.flatMap(_.varsExp)).distinct
    //    // we replace them with identitical variables that have different tags
    //    variables.map(vrr => SymReplace(vrr, Var(Identifier(vrr.id.name, Identity.tagged(tag)))))

    //def stripTags(pc: PathCondition, m: SymStore, d: Delta): Delta =
    //    // we get all the variables
    //    val variables = (pc.varsExp ++ SymbolicStore.variableExpressions(m) ++ d.flatMap(_.varsExp)).distinct
    //    // we replace them with identical variables that have no tags at all
    //    variables.map(vrr => SymReplace(vrr, Var(Identifier(vrr.id.name, Identity.none)))).distinct

    /**
     * We compute which variables need to be renamed by computing a set of changes.
     *
     * For a given pc, m, d and pc', m', d' if (pc, m, d) == (pc', m', d') modulo alpha renaming, it is garuantueed that this function computes a list
     * of changes for these parts that make them equal.
     *
     * @param pc
     *   the pathCondition to see which variables are available
     * @param m
     *   the symbolic store
     * @param d
     *   a set of changes
     */
    def computeRenaming(pc: PathCondition, m: SymStore, d: Delta): Delta =
        // compute the set of variables in all the parts
        val pcVariables = pc.vars
        val mVars = SymbolicStore.variables(m)
        val dVars = d.flatMap(_.vars)
        val allVars = (pcVariables ++ mVars ++ dVars).distinct.sortBy(_.split('x')(1).toInt)

        allVars.zip(0 to allVars.size).map { case (original, newidx) => SymReplace(VarId(original), VarId(s"x$newidx")) }

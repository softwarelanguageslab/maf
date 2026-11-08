package maf.semantics.scheme

import maf.domain.*
import maf.domain.lattice.*
import maf.domain.scheme.*
import maf.syntax.*
import maf.syntax.scheme.SchemeExp

trait CustomAllocator extends SchemeSemantics:
    anal: SchemeDomain with SchemeModFLocalSensitivity =>

    def allocVar(idf: Var, ctx: Ctx): Adr
    def allocPtr(ptr: SchemeExp[Any], ctx: Ctx): Adr

    implicit abstract override protected def analysisM: AnalysisM[A] =
        val superAnal = super.analysisM
        new AnalysisM[A]:
            export superAnal.*
            override def allocVar(vrb: Var) =
                map(getCtx)(anal.allocVar(vrb, _))
            override def allocPtr(exp: SchemeExp[Any]) =
                map(getCtx)(anal.allocPtr(exp, _))

// an allocator which allocates all variables with the same name to the same address
trait NameBasedAllocator extends CustomAllocator:
    this: SchemeDomain with SchemeModFLocalSensitivity =>
    // a single identifier per variable name, which represents all variables with that name
    private val names = scala.collection.mutable.Map.empty[String, Identifier]
    def allocVar(idf: Var, ctx: Ctx): Adr = VarAddr(names.getOrElseUpdate(idf.name, Identifier(idf.name, None)), ctx)
    def allocPtr(ptr: SchemeExp[Any], ctx: Ctx): Adr = PtrAddr(ptr, ctx)

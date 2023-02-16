package maf.modular.scheme.modflocal

import maf.core.*
import maf.modular.scheme.*
import maf.language.scheme.*

trait CustomAllocator extends SchemeSemantics:
    anal: SchemeDomain with SchemeModFLocalSensitivity =>

    def allocVar(idf: Var, ctx: Ctx): Adr
    def allocPtr(ptr: Exp, ctx: Ctx): Adr

    implicit abstract override protected def analysisM: AnalysisM[A] =
        val superAnal = super.analysisM
        new AnalysisM[A]:
            export superAnal.*
            override def allocVar(vrb: Var) =
                map(getCtx)(anal.allocVar(vrb, _))
            override def allocPtr(exp: Exp) =
                map(getCtx)(anal.allocPtr(exp, _))
            override def nontail[X](blk: => A[X]): A[X] = 
                superAnal.nontail(blk)

// an allocator which allocates all variables with the same name to the same address
trait NameBasedAllocator extends CustomAllocator:
    this: SchemeDomain with SchemeModFLocalSensitivity =>
    def allocVar(idf: Var, ctx: Ctx): Adr = VarAddr(Identifier(idf.name, Identity.none), ctx)
    def allocPtr(ptr: Exp, ctx: Ctx): Adr = PtrAddr(ptr, ctx)

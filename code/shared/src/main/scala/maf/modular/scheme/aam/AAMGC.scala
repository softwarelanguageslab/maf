package maf.modular.scheme.aam

import maf.modular.scheme.SchemeDomain

import maf.core.*
import maf.core.Lattice.*
import maf.util.Wrapper
import maf.lattice.interfaces.LatticeWithAddrs
import maf.language.scheme.SchemeExp

//
// Abstract GC
//

trait AAMGC extends AAMScheme:
    this: SchemeDomain with AAMSchemeSensitivity => 

    // stores: now with support for GC!
        
    given GC[Sto, Adr] = GC.storeStopAndCopyGC
    given GC[KSto, KAdr] = GC.storeStopAndCopyGC

    // implement refs functions

    implicit object KontLattice extends SetLattice[Frame] with LatticeWithAddrs[Kon, KAdr]:
        def refs(k: Kon): Set[KAdr] = k.map(_.aₖ)
    
    def refs(ρ: Env): Set[Adr] = ρ.addrs
    def refs(v: Val): Set[Adr] = lattice.refs(v)
    def refs(k: Kon): Set[Adr] = k.flatMap { 
        case IffK(c, a, ρ, t, aₖ) => refs(ρ)
        case SeqK(r, ρ, t, aₖ) => refs(ρ)
        case LetK(l, v, a, b, ρ, t, aₖ) => a.flatMap(b => refs(b._2)) ++ refs(ρ)
        case LtsK(l, v, b, ρ, t, aₖ) => refs(ρ)
        case LtrK(l, a, b, ρ, t, aₖ) => a :: l.map(_._1) ++ refs(ρ)
        case FunK(a, r, ρ, t, aₖ) => refs(ρ)
        case ArgK(a, f, v, r, ρ, t, aₖ) => refs(f) ++ v.flatMap(refs) ++ refs(ρ)
    }
    def refs(σₖ: KSto): Set[Adr] = σₖ.values.flatMap(refs)
    def refs(c: Control): Set[Adr] = c match
        case Ev(_, ρ, _) => refs(ρ)
        case Ko(v) => refs(v)
        case Ap(_, f, a, _) => refs(f) ++ a.flatMap(refs)
    
    // 

    // optimisation #1: restricting the environment to only free variables
    // this improves abstract GC, as there are less addresses in the root set (i.e., refs(ρ) is smaller)

    private def fv(bdy: List[Exp]): Set[String] =
        bdy.flatMap(_.fv).toSet

    def restrictEnv(ς: State): State = ς.copy(c = restrictEnv(ς.c))
    def restrictEnv(c: Control): Control = c match
        case Ev(e, ρ, t)    => Ev(e, ρ.restrictTo(e.fv), t)
        case _: Ko          => c
        case _: Ap          => c
    def restrictEnv(f: Frame): Frame = f match
        case IffK(c, a, ρ, t, aₖ)       => IffK(c, a, ρ.restrictTo(c.fv ++ a.fv), t, aₖ) 
        case SeqK(r, ρ, t, aₖ)          => SeqK(r, ρ.restrictTo(fv(r)), t, aₖ) 
        case LetK(l, v, a, b, ρ, t, aₖ) => LetK(l, v, a, b, ρ.restrictTo(fv(l.map(_._2) ++ b)), t, aₖ)
        case LtsK(l, v, b, ρ, t, aₖ)    => LtsK(l, v, b, ρ.restrictTo(fv(l.map(_._2) ++ b)), t, aₖ)
        case LtrK(l, a, b, ρ, t, aₖ)    => LtrK(l, a, b, ρ.restrictTo(fv(l.map(_._2) ++ b)), t, aₖ)
        case FunK(a, r, ρ, t, aₖ)       => FunK(a, r, ρ.restrictTo(fv(r)), t, aₖ)
        case ArgK(a, f, v, r, ρ, t, aₖ) => ArgK(a, f, v, r, ρ.restrictTo(fv(r)), t, aₖ)
    
    override protected def push(frm: Frame, e: SchemeExp, ρ: Environment[Address], t: Ctx, σ: Sto, σₖ: KSto): Set[State] =
        super.push(restrictEnv(frm), e, ρ, t, σ, σₖ)

    // optimisation #2: GC'ing the store and continuation store
     
    def gc(ς: State): State = 
        val State(c, σ, σₖ, aₖ) = ς
        val σₖ2 = σₖ.collect(Set(aₖ))
        val σ2 = σ.collect(refs(c) ++ refs(σₖ2))
        State(c, σ2, σₖ2, aₖ)
    
    // new step function integrates both optimisations
    override def step(ς: State): Set[State] = 
        super.step(ς)           // first do a normal step
             .map(restrictEnv)  // restrict the environment
             .map(gc)           // gc the stores
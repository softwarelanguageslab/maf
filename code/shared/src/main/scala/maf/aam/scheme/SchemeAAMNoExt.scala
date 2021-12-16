package maf.aam.scheme

trait SchemeAAMNoExt extends BaseSchemeAAMSemantics:
    type Ext = Unit
    type Val = LatVal
    def emptyExt: Ext = ()
    def inject(vlu: LatVal): Val = vlu
    def project(vlu: Val): LatVal = vlu

package maf.aam.scheme

trait SchemeAAMNoExt extends SchemeAAMSemantics:
    type Ext = Unit
    def emptyExt: Ext = ()

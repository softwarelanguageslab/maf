package maf.language.ContractScheme

import maf.language.scheme.primitives.BaseSchemePrelude
import maf.language.scheme.*
import maf.core.Position

object ContractSchemePrelude extends BaseSchemePrelude:
    override def primDefs = super.primDefs ++ List(
      "any/c" -> "(define any/c (flat (lambda (_) #t)))",
      "fprintf" -> "(define (fprintf . args) '())",
      "empty?" -> "(define empty? null?)",
      "and/c" -> "(define (and/c a b) (lambda (v) (and (check a v) (check b v))))",
      "listof" -> """(define (listof contract) 
        (define (iter xs)
          (if (null? xs)
            #t
            (and (check contract (car xs)) (iter (cdr xs)))))

        (and/c list? (flat (lambda (v) (iter v)))))"""
    )

    override def parseDef(dff: String, nam: String): List[SchemeExp] =
      List(ContractSchemeParser.compile(dff, Position.newTag(nam)))

end ContractSchemePrelude

package maf.language.symbolic

object DNF:
    import FormulaAux.*

    /** Convert a formula to a full disjunctive normal form */
    def dnf(formula: Formula): Formula = formula match
        case Disjunction(formulas) =>
          // for a disjunction to be in DNF its subformulas need to be in DNF ...
          val formulasDnf = formulas.map(dnf)
          // ... and the resulting formulas should be flattened as they could contain disjunctions as well.
          flatten(formulasDnf)

        case Conjunction(formulas) =>
          // for a conjunction to be in DNF, its subformulas must be in DNF ...
          val formulasDnf = formulas.map(dnf)
          // ... and their results should be distributed, such that:
          // (a \/ b) /\ (c \/ d) becomes (a /\ c) \/ (a /\ d) \/ (b /\ c) \/ (b /\ d)
          distribute(formulasDnf)

        // an assertions is already in DNF
        case a @ Assertion(_) => a

package maf.language.symbolic

object DNF:
    import FormulaAux.*

    /** Convert a formula to a full disjunctive normal form */
    private def _dnf(formula: Formula): Formula = formula match
        case Disjunction(formulas) =>
            // for a disjunction to be in DNF its subformulas need to be in DNF ...
            val formulasDnf = formulas.map(_dnf)
            // ... and the resulting formulas should be flattened as they could contain disjunctions as well.
            flatten(formulasDnf)

        case Conjunction(formulas) =>
            // for a conjunction to be in DNF, its subformulas must be in DNF ...
            val formulasDnf = formulas.map(_dnf)
            // ... and their results should be distributed, such that:
            // (a \/ b) /\ (c \/ d) becomes (a /\ c) \/ (a /\ d) \/ (b /\ c) \/ (b /\ d)
            distribute(formulasDnf.toList)

        // An empty formula is also already in DNF
        case EmptyFormula => EmptyFormula

        // an assertions is already in DNF
        case a @ Assertion(_) => a

    def filterDuplicates(formula: Formula, deep: Int = 0): Formula =
        def run(conjunctions: List[Formula], collected: Set[Formula] = Set()): List[Formula] = conjunctions match
            case f :: fs if !collected.contains(f) =>
                f :: run(fs, collected + f)
            case f :: fs =>
                run(fs, collected)
            case List() =>
                List()

        formula match
            case Disjunction(conjunctions) if deep == 0 => disj(run(conjunctions.map(filterDuplicates(_, deep + 1)).toList))
            case Conjunction(formulas) if deep < 2      => conj(run(formulas.toList, Set()))
            case (_: Disjunction) =>
                throw new Exception(s"disjunctions cannot be nested, current level $deep")
            case (_: Conjunction) =>
                throw new Exception(s"conjunctions cannot be nested, current level $deep")
            case v => v

    def dnf(formula: Formula): Formula = filterDuplicates(_dnf(formula))

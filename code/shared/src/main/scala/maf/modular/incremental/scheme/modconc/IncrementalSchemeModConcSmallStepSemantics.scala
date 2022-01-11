package maf.modular.incremental.scheme.modconc

import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeCodeChange
import maf.modular.incremental.IncrementalConfiguration.InvalidConfigurationException
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.scheme.ssmodconc.*

trait IncrementalSchemeModConcSmallStepSemantics extends SmallStepModConcSemantics with IncrementalSchemeSemantics:

    override def init(): ComponentContext =
        if configuration.cyclicValueInvalidation then
            throw new InvalidConfigurationException(
              s"$configuration not supported by Small-Step ModConc semantics (CY not supported - no implicit paths detected).",
              configuration
            )
        super.init()

    trait IncrementalSmallStepIntra extends SmallStepIntra with IncrementalIntraAnalysis:
        override protected def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match
            case SchemeCodeChange(e, _, _) if version == Old =>
              registerComponent(e, component)
              Set(Eval(e, env, stack))
            case SchemeCodeChange(_, e, _) if version == New =>
              registerComponent(e, component)
              Set(Eval(e, env, stack))
            case _ =>
              registerComponent(exp, component)
              super.evaluate(exp, env, stack)

    override def configString(): String = super.configString() + "\n  applying incremental ModConc Scheme semantics"

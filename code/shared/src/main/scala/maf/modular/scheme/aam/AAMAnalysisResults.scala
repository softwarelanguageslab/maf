package maf.modular.scheme.aam

import maf.core.*
import maf.modular.*
import maf.modular.scheme.*
import maf.language.scheme.*
import maf.util.Wrapper

trait AAMAnalysisResults extends AAMScheme with AnalysisResults[SchemeExp]:
    this: AAMSchemeSensitivity with SchemeDomain =>

    var resultsPerIdn = Map.empty.withDefaultValue(Set.empty)

    sealed trait InstrumentedStore[S] extends Store[S]
    given instrumented[S](using base: Store[S]): InstrumentedStore[S] with
        export base.{extend => _, update => _, _}
        extension (s: S)
            def extend(adr: Adr, vlu: Val) = 
                adr match
                    case _: VAdr | _: PAdr =>
                        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
                    case _ => ()
                base.extend(s)(adr, vlu)
            override def update(adr: Adr, vlu: Val) =
                adr match
                    case _: VAdr | _: PAdr=>
                        resultsPerIdn += adr.idn -> (resultsPerIdn(adr.idn) + vlu)
                    case _ => ()
                base.update(s)(adr, vlu)

    private lazy val baseWrapper = super.storeWrapper
    override def storeWrapper: Wrapper[InstrumentedStore] = new Wrapper:
        type T = baseWrapper.T
        lazy val instance = instrumented(using baseWrapper.instance)

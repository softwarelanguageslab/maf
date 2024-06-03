package maf.save

import io.bullet.borer.Decoder
import io.bullet.borer.Json
import maf.core.Expression
import java.nio.file.Paths
import java.nio.file.Files
import maf.language.scheme.SchemeExp
import io.bullet.borer.Reader
import scala.collection.mutable.HashMap
import maf.modular.AnalysisEntry
import maf.modular.ModAnalysis
import scala.collection.mutable.ListBuffer
import io.bullet.borer.Cbor

/**
 * Contains info about the top-level objects that need to be loaded.
 *
 * @param load
 *   Function that should be called with the value after it was decoded
 * @param decoder
 *   Decodes the value
 * @tparam T
 *   The type of the value the needs to be loaded
 */
case class Loadable[T](val load: (T) => Unit)(using val decoder: Decoder[T])

/**
 * The base trait for saving an analysis.
 *
 * Implementing this allows you to load your analysis, by default it will only load the name of your analysis and you should mixin other traits like
 * [[LoadComponents]] to also load components.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait Load[Expr <: Expression] extends AnalysisEntry[Expr]:
    /** Decode an analysis. */
    given analysisDecoder: Decoder[Load[Expr]] with
        override def read(reader: Reader): Load[Expr] =
            val loadInfo = Load.this.loadInfo
            loadSet = loadInfo.map(_._1).toSet
            reader.read[Load[Expr]]()
            loadSet = Set[String]()
            return Load.this

    protected var loadSet = Set[String]()
    protected given excludedAnalysisDecoder: MapDecoder[Load[Expr]] with
        override def read(reader: Reader): Load[Expr] =
            reader.start()
            var loadResults = ListBuffer[() => Unit]()
            for (key, value) <- loadInfo do
                if loadSet.contains(key) then
                    val result = reader.readMember(key)(using value.decoder)
                    if result.hasValue then value.load(result.value)
                    else loadResults = loadResults.addOne(() => value.load(result.value))
            for loadRes <- loadResults do loadRes()
            reader.close()
            return Load.this

    def startLoad(): Unit = return

    override def load(filename: String): Unit =
        startLoad()
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Json.decode(bytes).to[Load[Expr]](using analysisDecoder).value

    override def load(filename: String, load: Set[String]): Unit =
        startLoad()
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Json.decode(bytes).to[Load[Expr]](using excludedAnalysisDecoder).value

    /**
     * Returns a map strings and [[Loadable]] s.
     *
     * This map defines all top-level objects that should be loaded in your analysis, and the key with which they should be loaded. If you want to
     * load something else, you can override this method and add something to it.
     *
     * {{{
     * override def loadInfo: Map[String, Loadable[_]] =
     *     super.loadInfo + ("< key >" -> Loadable[< loadType >]((< loaded object >: < SaveType >) => < put loaded object into analysis >))
     * }}}
     */
    def loadInfo: List[(String, Loadable[_])] = List(("name", Loadable((name: String) => ())))

trait LoadCbor[Expr <: Expression] extends Load[Expr]:
    override def load(filename: String): Unit =
        startLoad()
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Cbor.decode(bytes).to[Load[Expr]](using analysisDecoder).value

    override def load(filename: String, load: Set[String]): Unit =
        startLoad()
        val bytes = Files.readAllBytes(Paths.get(filename))
        if bytes != null then Json.decode(bytes).to[Load[Expr]](using excludedAnalysisDecoder).value

trait LoadInitialized[Expr <: Expression] extends ModAnalysis[Expr] with Load[Expr]:
    override def loadInfo: List[(String, Loadable[?])] =
        super.loadInfo ++ List(
          ("initialized",
           Loadable((initialized: Boolean) =>
               if !visited.contains(initialComponent) then visited = visited + initialComponent
               analysisInitialized = initialized
           )
          )
        )

/** The trait used to load the modF analysis. */
trait LoadModF
    extends LoadCbor[SchemeExp]
    with LoadInitialized[SchemeExp]
    with LoadExpressionIntID[SchemeExp]
    with LoadSchemeExpressions
    with LoadComponents[SchemeExp]
    with LoadComponentIntID[SchemeExp]
    with LoadStandardSchemeComponents
    with LoadNoContext[SchemeExp]
    with LoadSchemeAddr
    with LoadDependency[SchemeExp]
    with LoadAddrDependency[SchemeExp]
    with LoadGlobalStore[SchemeExp]
    with LoadModularSchemeDomain

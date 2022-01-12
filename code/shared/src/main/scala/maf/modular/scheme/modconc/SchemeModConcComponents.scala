package maf.modular.scheme.modconc

import maf.core._
import maf.util._
import maf.language.scheme._
import maf.language.CScheme._

// A SchemeModConcComponent represents threads
sealed trait SchemeModConcComponent extends SmartHash with TID
// The main thread
case object MainThread extends SchemeModConcComponent:
    override def toString: String = "main-thread"
// A thread that was spawned in some component
case class Thread[Context](
    exp: SchemeExp,
    env: Environment[Address],
    ctx: Context)
    extends SchemeModConcComponent:
    override def toString: String = s"thread@${exp.idn}"

trait StandardSchemeModConcComponents extends SchemeModConcSemantics:
    type Component = SchemeModConcComponent
    lazy val initialComponent = MainThread
    def newComponent(thread: Thread[ComponentContext]) = thread
    def view(cmp: Component) = cmp
    override def configString(): String = super.configString() + "\n  having standard scheme ModConc components"

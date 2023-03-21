package maf.language.scheme.interpreter

import maf.core.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.util.*
import maf.util.benchmarks.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.*
import scala.util.control.TailCalls.*

class SmartReplaceInterpreter(cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
                              io: IO = new EmptyIO()) extends SchemeInterpreter(cb, io):
  import ConcreteValues._
  import scala.util.control.TailCalls._

  private var dynAnalysis: Map[SchemeExp, Set[Value]] = Map()

  def runAndProfile(program: SchemeExp,
                    timeout: Timeout.T,
                    version: Version = New): (Value, Map[SchemeExp, Set[Value]]) =
    setStore(initialSto)
    dynAnalysis = Map()
    (eval(program, initialEnv, timeout, version).result, dynAnalysis)

  override def eval(e: SchemeExp, env: Env, timeout: Timeout.T, version: Version): TailRec[Value] =
    val value = super.eval(e, env, timeout, version)
    dynAnalysis = dynAnalysis + (e -> (dynAnalysis.getOrElse(e, Set()) + value.result))
    value

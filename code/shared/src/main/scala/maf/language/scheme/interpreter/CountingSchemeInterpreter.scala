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

class CountingSchemeInterpreter(cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
                                io: IO = new EmptyIO()) extends SchemeInterpreter(cb, io):
  import ConcreteValues._

  import scala.util.control.TailCalls._

  /** Evaluate with a Bound on Evaluation Steps */
  private var evalSteps: Long = 0

  def getEvalSteps(): Long = evalSteps

  var maxEvalSteps: Long = Long.MaxValue //the maximum number of eval steps before a TimeoutException
  var buffer: Int = 10000 //buffer has to be sufficiently large

  def runWithMaxSteps(
                       program: SchemeExp,
                       timeout: Timeout.T,
                       maxSteps: Long,
                       version: Version = New
                     ): Value =
    setStore(initialSto)
    maxEvalSteps = maxSteps
    evalSteps = 0
    eval(program, initialEnv, timeout, version).result

  override def eval(e: SchemeExp, env: Env, timeout: Timeout.T, version: Version): TailRec[Value] =
    evalSteps += 1
    if (evalSteps - buffer) > maxEvalSteps then
      throw new TimeoutException()
    super.eval(e, env, timeout, version)

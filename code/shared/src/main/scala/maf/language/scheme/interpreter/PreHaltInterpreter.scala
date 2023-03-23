package maf.language.scheme.interpreter

import maf.core.*
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.util.*
import maf.util.benchmarks.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.*
import scala.util.control.TailCalls.*

class PreHaltInterpreter(cb: (Identity, ConcreteValues.Value) => Unit = (_, _) => (),
                         io: IO = new EmptyIO(),
                         problematicValue: ConcreteValues.Value,
                         soundnessTest: () => Boolean
                        ) extends SchemeInterpreter(cb, io):

  import ConcreteValues._
  import scala.util.control.TailCalls._

  override def run(program: SchemeExp, timeout: Timeout.T, version: Version = New): Value =
    try
      setStore(initialSto)
      eval(program, initialEnv, timeout, version).result
    catch
      case e: InterruptedException =>
        Value.Integer(1)

  override def extendStore(a: Addr, v: Value): Unit =
    if checkAddr(a) && checkValue(v) then Callback.call(a._2.idn, v)
    if v equals problematicValue then
      if soundnessTest() then
        throw InterruptedException()
    super.extendStore(a, v)





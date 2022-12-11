package maf.test.TurgutsThesis.soundness.dd.evaluation.baseline

import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.ProfilingDataCollector

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

object BaselineDataCollector:
  def readObject(suffix: String): BaselineDataCollector =
    val ois = new ObjectInputStream(new FileInputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/evaluationLogs/objects/" + suffix))
    val o = ois.readObject.asInstanceOf[BaselineDataCollector]
    ois.close()
    o

class BaselineDataCollector extends Serializable:
  var originalSizes: Map[String, List[Int]] = Map()
  def addOriginalSize(bugName: String, size: Int) =
    val current = originalSizes.getOrElse(bugName, List())
    originalSizes = originalSizes + (bugName -> current.::(size))

  var reducedSizes: Map[String, List[Int]] = Map()
  def addReducedSize(bugName: String, reducedSize: Int) =
    val current = reducedSizes.getOrElse(bugName, List())
    reducedSizes = reducedSizes + (bugName -> current.::(reducedSize))

  var reductionTimes: Map[String, List[Long]] = Map()
  def addReductionTime(bugName: String, time: Long) =
    val current = reductionTimes.getOrElse(bugName, List())
    reductionTimes = reductionTimes + (bugName -> current.::(time))

  def writeTo(suffix: String): Unit =
    val oos = new ObjectOutputStream(new FileOutputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/evaluationLogs/objects/" + suffix))
    oos.writeObject(this)
    oos.close()

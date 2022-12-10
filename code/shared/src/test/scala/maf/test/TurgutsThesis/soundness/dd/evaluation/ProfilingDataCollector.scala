package maf.test.TurgutsThesis.soundness.dd.evaluation

import java.io.{ByteArrayOutputStream, ObjectOutputStream, FileOutputStream, ObjectInputStream, FileInputStream}

object ProfilingDataCollector:
  def readObject(suffix: String): ProfilingDataCollector =
    val ois = new ObjectInputStream(new FileInputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/fileLogs/objects/" + suffix))
    val o = ois.readObject.asInstanceOf[ProfilingDataCollector]
    ois.close()
    o

class ProfilingDataCollector extends Serializable:
  private var id = 0
  def newID(): Unit =
    id += 1

  var candidateFunctionCount: Map[Int, Int] = Map()
  def addCandidateFunctionCount(count: Int) =
    candidateFunctionCount = candidateFunctionCount + (id -> count)

  var functionCount: Map[Int, Int] = Map()
  def addFunctionCount(count: Int) =
    functionCount = functionCount + (id -> count)

  var oracleCounts: Map[Int, Int] = Map()
  def addOracleCount(count: Int) =
    oracleCounts = oracleCounts + (id -> count)

  var programSizes: Map[Int, Int] = Map()
  def addProgramSize(size: Int) =
    programSizes = programSizes + (id -> size)

  var reducedSizes: Map[Int, Int] = Map()
  def addReducedSize(size: Int) =
    reducedSizes = reducedSizes + (id -> size)

  var oracleHits: Map[Int, Int] = Map()
  def addOracleHit(hit: Int) =
    oracleHits = oracleHits + (id -> hit)

  var reductionTimes: Map[Int, Long] = Map()
  def addReductionTime(time: Long) =
    reductionTimes = reductionTimes + (id -> time)

  var oracleTimes: Map[Int, List[(Long, Int)]] = Map()
  def addOracleTimes(times: List[(Long, Int)]) =
    oracleTimes = oracleTimes + (id -> times)

  var analysisSteps: Map[Int, List[(Int, Int)]] = Map()
  def addAnalysisSteps(steps: List[(Int, Int)]) =
    analysisSteps = analysisSteps + (id -> steps)

  var interpreterSteps: Map[Int, List[(Int, Int)]] = Map()
  def addInterpreterSteps(steps: List[(Int, Int)]) =
    interpreterSteps = interpreterSteps + (id -> steps)

  def writeTo(suffix: String): Unit =
    // (2) write the instance out to a file
    val oos = new ObjectOutputStream(new FileOutputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/fileLogs/objects/" + suffix))
    oos.writeObject(this)
    oos.close()
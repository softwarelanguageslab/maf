package maf.test.TurgutsThesis.soundnessDD.evaluation.profiling

import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

case class ReductionData(candidateFunctionCount: Int,
                         totalFunctionCount: Int,
                         oracleCount: Int,
                         programSize: Int,
                         reducedSize: Int,
                         OracleHits: Int,
                         reductionTime: Long,
                         oracleTimes: List[(Long, Int)],
                         analysisSteps: List[(Int, Int)]) extends Serializable

object ProfilingDataCollector:
  def readObject(suffix: String): ProfilingDataCollector =
    val ois = new ObjectInputStream(new FileInputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/evaluationLogs/objects/" + suffix))
    val o = ois.readObject.asInstanceOf[ProfilingDataCollector]
    ois.close()
    o

class ProfilingDataCollector extends Serializable:
  var data: List[ReductionData] = List()
  def addReductionData(d: ReductionData) =
    data = data.::(d)
    
  def filter(minOriginalSize: Int): ProfilingDataCollector =
    val res = new ProfilingDataCollector
    res.data = data.filter(r => r.programSize > minOriginalSize)
    res

  def writeTo(suffix: String): Unit =
    val oos = new ObjectOutputStream(new FileOutputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/evaluationLogs/objects/" + suffix))
    oos.writeObject(this)
    oos.close()
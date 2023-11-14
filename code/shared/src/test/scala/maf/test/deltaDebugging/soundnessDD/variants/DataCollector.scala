package maf.test.deltaDebugging.soundnessDD.variants

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

case class ReductionData(benchmark: String,
                         bugName:String,
                         origSize: Int,
                         reducedSize: Int,
                         reductionPercentage: Double,
                         reductionTime: Long,
                         oracleTreeSizes: List[Int]
                        ) extends Serializable

object DataCollector:
  def readObject(suffix: String): DataCollector =
    val ois = new ObjectInputStream(new FileInputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/evaluationLogs/objects/" + suffix))
    val o = ois.readObject.asInstanceOf[DataCollector]
    ois.close()
    o

class DataCollector extends Serializable:
  var reductionData: List[ReductionData] = List()

  def addReductionData(r: ReductionData) =
    reductionData = reductionData.::(r)

  def writeTo(suffix: String): Unit =
    val oos = new ObjectOutputStream(new FileOutputStream("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/evaluationLogs/objects/" + suffix))
    oos.writeObject(this)
    oos.close()
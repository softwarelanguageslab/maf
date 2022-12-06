package maf.test.TurgutsThesis.soundness.dd.evaluation

class DataCollector:

  var oracleCounts: List[Int] = List()
  def addOracleCount(count: Int) =
    oracleCounts = oracleCounts.::(count)
  
  var programSizes: List[Int] = List()
  def addProgramSize(size: Int) =
    programSizes = programSizes.::(size)
  
  var reducedSizes: List[Int] = List()
  def addReducedSize(size: Int) =
    reducedSizes = reducedSizes.::(size)

  var reductionTimes: List[Long] = List()
  def addReductionTime(time: Long) =
    reductionTimes = reductionTimes.::(time)
  
  var oracleTimes: List[(Long, Int)] = List()
  def addOracleTimes(time: (Long, Int)) =
    oracleTimes = oracleTimes.::(time)
    
  var analysisSteps: List[(Int, Int)] = List()
  def addAnalysisSteps(steps: (Int, Int)) =
    analysisSteps = analysisSteps.::(steps)
    
  var interpreterSteps: List[(Int, Int)] = List()
  def addInterpreterSteps(steps: (Int, Int)) =
    interpreterSteps = interpreterSteps.::(steps)

  
  
    
    


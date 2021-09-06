package maf.util.benchmarks

// Helpers to compute statistics over sets of data
object Statistics:

    /**
     * Bundles multiple statitistical measures in one object.
     *
     * @param min
     *   The minimum value measured.
     * @param max
     *   The maximum value measured.
     * @param mean
     *   The mean value of measurements.
     * @param median
     *   The median value of measurements.
     * @param stddev
     *   The standard deviation of measurements.
     */
    case class Stats(
        min: Double,
        max: Double,
        mean: Double,
        median: Double,
        stddev: Double):
        override def toString: String =
          s"* Values in [$min,$max]\n" ++
            s"* Mean: $mean , Median: $median\n" ++
            s"* Standard deviation: $stddev"

    /** Computes the mean value of a list of measurements. */
    def mean(l: List[Double]): Double = l.sum / l.length

    /** Computes the median value of a list of measurements. */
    def median(l: List[Double]): Double =
        val s = l.sorted
        val split: Int = s.length / 2
        if s.length % 2 == 0 then (s(split - 1) + s(split)) / 2 else s(split)

    /** Computes the standard deviation of a list of measurements. */
    def stddev(l: List[Double]): Double =
      if l.length == 1 then 0
      else
          val mea: Double = mean(l)
          val sqDiffs: List[Double] = l.map(v => scala.math.pow(v - mea, 2))
          scala.math.sqrt(sqDiffs.sum / (l.length - 1))

    /** Computes all statistics of a list of measurements and returns an object containing these statistics. */
    def all(l: List[Double]): Stats = Stats(l.min, l.max, mean(l), median(l), stddev(l))

    /**
     * Computes the weighted average of a list of tuples (weight, value).
     * @param l
     *   A list of tuples (weight, value).
     * @return
     *   The weighted average of the input list.
     */
    def weightedAverage(l: List[(Double, Double)]): Double =
        val num = weightedSum(l)
        val den = l.foldLeft(0.0)(_ + _._1)
        num / den

    /**
     * Computes the weighted sum of a list of tuples (weight, value).
     * @param l
     *   A list of tuples (weight, value).
     * @return
     *   The weighted sum of the input list.
     */
    def weightedSum(l: List[(Double, Double)]): Double =
      l.foldLeft(0.0)((acc, t) => acc + t._1 * t._2)

package coursera.fundesigninscala.week1.generator

/**
  * Created by albert on 5/02/17.
  */
object RandomTestHelper {
  def checkRandomness[T](errorThresholdTolerance: Int = 2, vals: Seq[T]): Boolean = {
    val totalOfElements: Int = vals.length
    val numOfDistinctElements: Int = vals.toList.distinct.length
    val percentageOfErrors = 100 - ((numOfDistinctElements * 100) / totalOfElements)
    percentageOfErrors < errorThresholdTolerance
  }

  // TODO: that function could be improved
  def checkRandomnessOfSet[T](errorThresholdTolerance: Int = 10, vals: Seq[T], set: Set[T]): Boolean = {

    val numOfDistinctElements: Int = vals.toList.distinct.length
    val percentageOfErrors = 100 - ((numOfDistinctElements * 100) / set.size)
    percentageOfErrors < errorThresholdTolerance
  }

  def executeNTimes[T](times: Int = 100, expr: => T): Seq[T] = (1 to times).map(_ => expr)

}

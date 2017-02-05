package coursera.fundesigninscala.week1.generator

/**
  * Created by albert on 5/02/17.
  */
object RandomTestHelper {
  def checkRandomness[T](errorThresholdTolerance:Int=2,vals:Seq[T]) : Boolean = {
    val totalOfElements: Int = vals.length
    val numOfDistinctElements: Int = vals.toList.distinct.length
    val percentageOfErrors = 100 - ((numOfDistinctElements * 100) / totalOfElements)
    percentageOfErrors < errorThresholdTolerance
  }

  def checkRandomnessOfBooleans(errorThresholdTolerance:Int=80,vals:Seq[Boolean]) : Boolean = {
    val totalOfElements: Int = vals.length
    val numberOfTrues: Int = vals.count(_ == true)
    val numberOfFalses: Int = totalOfElements - numberOfTrues
    val percentageOfErrors = 100 - ((numberOfTrues * 100) / totalOfElements)
    percentageOfErrors < errorThresholdTolerance
  }

}

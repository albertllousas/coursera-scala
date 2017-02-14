package week2.casestudy

import org.scalatest.{FunSuite, Matchers}




/**
  * Created by albert on 9/02/17.
  */
class WaterPouringProblemTest extends FunSuite with Matchers {

  val problem = new WaterPouringProblem(Vector(4,9))

  test("it should create all possible moves") {

    // given
    val possiblesMoves = Vector(problem.Empty(0), problem.Empty(1), problem.Fill(0), problem.Fill(1), problem.Pour(0,1), problem.Pour(1,0))

    // then
    problem.allPossiblesMoves should equal (possiblesMoves)

  }

  test("it should find all possible solutions") {
    val problemWith = new WaterPouringProblem(Vector(4,9))
    // then
    println(problemWith.solutions(6))

  }

}

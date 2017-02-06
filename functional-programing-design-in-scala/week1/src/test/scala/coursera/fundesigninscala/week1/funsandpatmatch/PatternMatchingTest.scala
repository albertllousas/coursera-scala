package coursera.fundesigninscala.week1.funsandpatmatch

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 6/02/17.
  */
class PatternMatchingTest extends FunSuite with Matchers {

  val data = JObj(
    Map(
      "firstName" -> JStr("John"),
      "lastName" -> JStr("Smith"),
      "address" -> JObj(
        Map(
          "streetAddress" -> JStr("21 2nd Street"),
          "state" -> JStr("NY"),
          "postalCode" -> JNum(10021)
        )
      )

    )
  )

  test("it should generate a string representation from JSON object using pattern matching") {

    // when
    val result:String = JSON.show(data)

    // then
    result should equal("{\"firstName\": \"John\", \"lastName\": \"Smith\", \"address\": {\"streetAddress\": \"21 2nd Street\", \"state\": \"NY\", \"postalCode\": 10021.0}}")
  }

}

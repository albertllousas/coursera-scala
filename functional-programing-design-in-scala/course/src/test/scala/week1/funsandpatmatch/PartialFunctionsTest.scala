package week1.funsandpatmatch

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 6/02/17.
  */
class PartialFunctionsTest extends FunSuite with Matchers {

  test("partial function should be defined in compact case (syntactic sugar)") {

    // given
    val compact: PartialFunction[String,String] = { case "ping" => "pong" }
    val expanded: PartialFunction[String,String] = new PartialFunction[String,String] {
      override def isDefinedAt(x: String): Boolean = x match {
        case "ping" => true
        case _ => false
      }

      override def apply(x: String): String = x match {
        case "ping" => "pong"
      }
    }

    // then
    compact.isDefinedAt("ping") should equal (true)
    expanded.isDefinedAt("pong") should equal (false)
    compact.isDefinedAt("ping") should equal (true)
    expanded.isDefinedAt("pong") should equal (false)
    compact("ping") should equal(expanded("ping"))

  }


}

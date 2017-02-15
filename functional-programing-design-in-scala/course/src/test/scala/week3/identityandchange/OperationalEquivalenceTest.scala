package week3.identityandchange

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 14/02/17.
  */
class OperationalEquivalenceTest extends FunSuite  with Matchers {

  val arbitrarySequenceOfOperations: (BankAccount,BankAccount) => Unit = (x, y) => {
    x deposit 30
    y withdraw 20
  }

  test("it should demonstrate that two Bank Accounts are different using Operational Equivalence") {

    // given
    val x = new BankAccount
    val y = new BankAccount


    // then
    the [Error] thrownBy arbitrarySequenceOfOperations(x,y) should have message "insufficient funds"
    noException should be thrownBy arbitrarySequenceOfOperations(x,x)


  }

  test("it should demonstrate that two Bank Accounts are maybe the same using Operational Equivalence") {

    // given
    val x = new BankAccount
    val y = x


    // then
    noException should be thrownBy arbitrarySequenceOfOperations(x,y)
    noException should be thrownBy arbitrarySequenceOfOperations(x,x)


  }

}

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Seq

/**
  * Created by albert on 8/02/17.
  */
class FoldTesting extends FunSuite with Matchers {

  def foldLeft[A, B](seq: Seq[A], z: B)(f: (B, A) => B): B =
    seq match {
      case Nil => z
      case x :: xs =>
        foldLeft(xs, f(z, x))(f) // (((1)+ 2) +3)

    }

  def foldRight[A, B](seq: Seq[A], z: B)(f: (A, B) => B): B =
    seq match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))  // (1+(2 +( 3)))

    }


  ignore("it should foldLeft a list summinng elements") {

        foldLeft(List(1,2,3),0){(a,b)=>a+b}

        foldRight(List(1,2,3),0){(a,b)=>a+b}

  }


}

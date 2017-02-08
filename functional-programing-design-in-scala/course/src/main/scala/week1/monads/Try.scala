package week1.monads

/**
  * Created by albert on 4/02/17.
  */
abstract class Try[+T] {

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try {
      f(x)
    }
    case fail: Failure => fail
  }

  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch {
      case ex: Exception => Failure(ex)
    }
    case fail: Failure => fail
  }
}

case class Success[T](x: T) extends Try[T]

case class Failure(ex: Exception) extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case ex: Exception => Failure(ex)
    }
}

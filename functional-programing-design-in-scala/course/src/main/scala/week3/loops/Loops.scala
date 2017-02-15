package week3.loops

/**
  * Created by aortiz on 15/02/17.
  */
object Loops {

  def WHILE(condition: => Boolean)(block: => Unit):Unit  = {
    if(condition) {
      block
      WHILE (condition) (block)
    } else ()
  }

}

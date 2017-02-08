package week2.casestudy

import scala.collection.immutable.IndexedSeq

/**
  * Created by aortiz on 8/02/17.
  */
class Pouring(capacityOfGlasses: Vector[Int]) {

  // states
  type State = Vector[Int]
  val initialState: Vector[Int] = capacityOfGlasses map (_ => 0)

  // Moves

  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, capacityOfGlasses(glass))
  }

  case class Pour(fromGlass: Int, toGlass: Int) extends Move {
    override def change(state: State): State = {
      val currentAvailableCapacityOnToGlass = capacityOfGlasses(toGlass) - state(toGlass)
      val amount = state(fromGlass) min currentAvailableCapacityOnToGlass

    }
  }


  val glasses: Range = 0 until capacityOfGlasses.length

  val allPossiblesMoves: IndexedSeq[Move] =
    (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass <- glasses) yield Fill(glass)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))

}

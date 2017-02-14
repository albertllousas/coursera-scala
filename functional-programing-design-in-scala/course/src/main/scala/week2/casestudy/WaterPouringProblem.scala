package week2.casestudy

import scala.collection.immutable.IndexedSeq

/**
  * Created by aortiz on 8/02/17.
  */
class WaterPouringProblem(capacityOfGlasses: Vector[Int]) {

  // States

  // currentState is a vector representing the quantity of each glass in a given moment
  type State = Vector[Int]
  val initialState: Vector[Int] = capacityOfGlasses map (_ => 0)

  // all zeros

  // Moves

  trait Move {
    def change(currentState: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(currentState: State): State = currentState updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(currentState: State): State = currentState updated(glass, capacityOfGlasses(glass))
  }

  case class Pour(fromGlass: Int, toGlass: Int) extends Move {
    override def change(currentState: State): State = {
      val currentAvailableCapacityOnToGlass = capacityOfGlasses(toGlass) - currentState(toGlass)
      val amountToPour = currentState(fromGlass) min currentAvailableCapacityOnToGlass
      currentState updated(fromGlass, currentState(fromGlass) - amountToPour) updated(toGlass, currentState(toGlass) + amountToPour)
    }
  }


  val glasses: Range = 0 until capacityOfGlasses.length

  val allPossiblesMoves: IndexedSeq[Move] =
    (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass <- glasses) yield Fill(glass)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))


  // paths

  class Path(history: List[Move], val endState:State) {

//    def endState: State = history.foldRight(initialState)(_ change _)

    //    def endState: State = trackState(history)
    //
    //    private def trackState(history: List[Move]):State = history match {
    //        case Nil => initialState
    //        case move :: restOfHistory => move change trackState(restOfHistory) // (n change (n-1 change (n-2 change ( n-3 change initialState))))
    //    }
    def extend(move: Move): Path = new Path(move :: history, move change endState)

    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath: Path = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more: Set[Path] = for {
        path <- paths
        next <- allPossiblesMoves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solutions(target:Int):Stream[Path] =
    for{
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path

}

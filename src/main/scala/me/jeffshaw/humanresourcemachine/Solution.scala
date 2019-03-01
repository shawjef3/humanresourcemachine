package me.jeffshaw.humanresourcemachine

import scala.annotation.tailrec
import scalaz.Scalaz._
import scalaz._

final case class State(
  in: Vector[Int],
  out: Vector[Int],
  hands: Option[Int],
  board: Vector[Option[Int]],
  here: Int
)

object State {
  def init(
    in: Vector[Int]
  ): State = {
    State(
      in = in,
      out = Vector(),
      hands = None,
      board = Vector.fill(10)(None),
      here = 0
    )
  }

  def init(
    condition: Level.Condition
  ): State = {
    init(condition.in)
  }
}

final case class Level(
  conditions: Set[Level.Condition]
)

object Level {
  case class Condition(in: Vector[Int], expectedOut: Vector[Int])
}

final case class Solution(
  level: Level,
  instructions: Vector[Solution.Instruction]
) {
  /**
    * Run one instruction.
    * @param state
    * @return either the final game state, which is an error message or the final state,
    *         or the next state.
    */
  def step(condition: Level.Condition, state: State): Disjunction[Disjunction[String, State], State] = {
    for {
      instruction <- instructions.lift(state.here).toRightDisjunction("invalid here".left)
      result <-
        instruction.run(level, state) match {
          case error: DLeft[String] =>
            error.left
          case finalState@DRight(result) =>
            if (result.out == condition.expectedOut) {
              finalState.left
            } else result.right
        }
    } yield result
  }

  lazy val run: Map[Level.Condition, Disjunction[(Vector[State], String), Vector[State]]] = {
    for (condition@Level.Condition(in, _) <- level.conditions.iterator) yield {
      val initialState = State.init(in)
      val result = run(condition, initialState, Vector.empty)
      condition -> result
    }
  }.toMap

  /**
    *
    * @return the history and the error, or the history and the current state.
    */
  @tailrec
  def run(condition: Level.Condition, state: State, history: Vector[State]): Disjunction[(Vector[State], String), Vector[State]] = {
    val result = step(condition, state)
    result match {
      case DLeft(DLeft(error)) =>
        (history :+ state, error).left
      case DLeft(DRight(finalState)) =>
        (history :+ finalState).right
      case DRight(nextState) =>
        run(condition, nextState, history :+ state)
    }
  }

  def printInstructions(): Unit = {
    for ((i, ix) <- instructions.zipWithIndex) {
      println(s"${ix+1}: ${i.gameValue}")
    }
  }
}

object Solution {

  type Steps = List[State]

  type Result = Disjunction[String, State]

  val handsAreEmpty: DLeft[String] = DLeft("hands are empty")
  
  private val outOfBoundsMemo = collection.mutable.Map[Int, DLeft[String]]()

  def outOfBounds(i: Int): DLeft[String] = {
    outOfBoundsMemo.getOrElseUpdate(i, DLeft(s"board($i) is out of bounds"))
  }

  private val isEmptyMemo = collection.mutable.Map[Int, DLeft[String]]()

  def isEmpty(i: Int): DLeft[String] = {
    isEmptyMemo.getOrElseUpdate(i, DLeft(s"board($i) is empty"))
  }
  
  sealed trait Instruction {
    def run(level: Level, state: State): Result

    val gameValue: String
  }

  implicit class ToRightMemo[A](val opt: Option[A]) extends AnyVal {
    def toRightMemo[B](left: DLeft[B]): Disjunction[B, A] = {
      opt match {
        case Some(value) =>
          value.right
        case None =>
          left
      }
    }
  }
  
  case object Inbox extends Instruction {
    val error: DLeft[String] = DLeft("empty inbox")

    override def run(level: Level, state: State): Result = {
      for {
        head <- state.in.headOption.toRightMemo(error)
      } yield {
        state.copy(
          hands = Some(head),
          here = state.here + 1,
          in = state.in.tail
        )
      }
    }

    override val gameValue: String = "inbox"
  }

  case object Outbox extends Instruction {
    override def run(level: Level, state: State): Result = {
      state.hands.fold[Result](handsAreEmpty) { value =>
        state.copy(
          out = value +: state.out,
          hands = None,
          here = state.here + 1
        ).right
      }
    }

    override val gameValue: String = "outbox"
  }

  case class CopyFrom(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        maybeValue <- state.board.lift(i).toRightMemo(outOfBounds(i))
        value <- maybeValue.toRightMemo(isEmpty(i))
      } yield state.copy(
        hands = Some(value),
        here = state.here + 1
      )
    }

    override val gameValue: String = s"copyfrom $i"
  }

  case class CopyTo(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      state.hands.fold[Result](handsAreEmpty) { value =>
        state.copy(
          board = state.board.updated(i, Some(value)),
          here = state.here + 1
        ).right
      }
    }

    override val gameValue: String = s"copyto $i"
  }

  case class Add(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        hands <- state.hands.toRightMemo(handsAreEmpty)
        maybeValue <- state.board.lift(i).toRightMemo(outOfBounds(i))
        board <- maybeValue.toRightMemo(isEmpty(i))
      } yield {
        state.copy(
          hands = Some(hands + board),
          here = state.here + 1
        )
      }
    }

    override val gameValue: String = s"add $i"
  }

  case class Sub(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        hands <- state.hands.toRightMemo(handsAreEmpty)
        maybeValue <- state.board.lift(i).toRightMemo(outOfBounds(i))
        board <- maybeValue.toRightMemo(isEmpty(i))
      } yield {
        state.copy(
          hands = Some(hands - board),
          here = state.here + 1
        )
      }
    }

    override val gameValue: String = s"sub $i"
  }

  case class BumpUp(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        maybeValue <- state.board.lift(i).toRightMemo(outOfBounds(i))
        value <- maybeValue.toRightMemo(isEmpty(i))
      } yield {
        val incrementedValue = Some(value + 1)
        state.copy(
          board = state.board.updated(i, incrementedValue),
          hands = incrementedValue,
          here = state.here + 1
        )
      }
    }

    override val gameValue: String = s"bump+ $i"
  }

  case class BumpDown(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        maybeValue <- state.board.lift(i).toRightMemo(outOfBounds(i))
        value <- maybeValue.toRightMemo(isEmpty(i))
      } yield {
        val decrementedValue = Some(value - 1)
        state.copy(
          board = state.board.updated(i, decrementedValue),
          hands = decrementedValue,
          here = state.here + 1
        )
      }
    }

    override val gameValue: String = s"bump- $i"
  }

  case class Jump(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      state.copy(
        here = i
      ).right
    }

    override val gameValue: String = s"jump ${i+1}"
  }

  case class JumpIfZero(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        hands <- state.hands.toRightMemo(handsAreEmpty)
      } yield {
        if (hands == 0) {
          state.copy(
            here = i
          )
        } else state.copy(
          here = state.here + 1
        )
      }
    }

    override val gameValue: String = s"jump if zero ${i+1}"
  }

  case class JumpIfNegative(i: Int) extends Instruction {
    override def run(level: Level, state: State): Result = {
      for {
        hands <- state.hands.toRightMemo(handsAreEmpty)
      } yield {
        if (hands < 0) {
          state.copy(
            here = i
          )
        } else state.copy(
          here = state.here + 1
        )
      }
    }

    override val gameValue: String = s"jump if negative ${i+1}"
  }

}

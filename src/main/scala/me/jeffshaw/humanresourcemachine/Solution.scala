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
  instructions: Vector[Instruction]
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

  lazy val run: Map[Level.Condition, Disjunction[(Steps, String), Steps]] = {
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
  def run(condition: Level.Condition, state: State, history: Vector[State]): Disjunction[(Steps, String), Steps] = {
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

}

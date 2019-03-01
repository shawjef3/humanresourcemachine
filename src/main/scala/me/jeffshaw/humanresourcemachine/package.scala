package me.jeffshaw

import scalaz.Scalaz._
import scalaz._

package object humanresourcemachine {

  type Steps = Vector[State]

  type Result = Disjunction[String, State]

  private val handsAreEmpty: DLeft[String] = DLeft("hands are empty")

  private def outOfBounds(i: Int): DLeft[String] = {
    DLeft(s"board($i) is out of bounds")
  }

  private val isEmptyMemo = Vector.tabulate(10)(i => DLeft(s"board($i) is empty"))

  private def isEmpty(i: Int): DLeft[String] = {
    isEmptyMemo.lift(i).getOrElse(DLeft(s"board($i) is empty"))
  }

  sealed trait Instruction {
    def run(level: Level, state: State): Result

    val gameValue: String
  }

  private implicit class ToRightMemo[A](val opt: Option[A]) extends AnyVal {
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
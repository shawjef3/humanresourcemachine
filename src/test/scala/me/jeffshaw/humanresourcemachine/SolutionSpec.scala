package me.jeffshaw.humanresourcemachine

import org.scalatest.FunSuite

class SolutionSpec extends FunSuite {

  test("countdown") {

    val condition = {
      val in = Vector(9,-5,0,2)

      val zero = Vector(0)

      def expectedOut(value: Int): Vector[Int] = {
        if (value == 0) {
          zero
        } else {
          Vector.tabulate(value.abs + 1)(i =>
            if (value > 0) {
              value - i
            } else value + i
          )
        }
      }.reverse

      Level.Condition(
        in = in,
        expectedOut = in.map(expectedOut).reverse.flatten
      )
    }

    val level =
      Level(Set(condition))

    val solution =
      Solution(
        level = level,
        instructions = Vector(
          Inbox,
          CopyTo(0),
          Outbox,
          CopyFrom(0),
          JumpIfZero(0),
          JumpIfNegative(11),
          BumpDown(0),
          Outbox,
          CopyFrom(0),
          JumpIfZero(0),
          Jump(3),
          // negative branch
          CopyFrom(0),
          BumpUp(0),
          Outbox,
          CopyFrom(0),
          JumpIfZero(0),
          Jump(11)
        )
      )

    val result = solution.run

    assertResult(condition.expectedOut)(result(condition).fold(_ => ???, states => states.last.out))
  }

}

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
    /* 0*/Inbox,
    /* 1*/CopyTo(0),
    /* 2*/Outbox,
    /* 3*/CopyFrom(0),
    /* 4*/JumpIfZero(0),
    /* 5*/JumpIfNegative(11),
    /* 6*/BumpDown(0),
    /* 7*/Outbox,
    /* 8*/CopyFrom(0),
    /* 9*/JumpIfZero(0),
    /*10*/Jump(3),
          // negative branch
    /*11*/CopyFrom(0),
    /*12*/BumpUp(0),
    /*13*/Outbox,
    /*14*/CopyFrom(0),
    /*15*/JumpIfZero(0),
    /*16*/Jump(11)
        )
      )

    assertResult(1)(solution.results)
    assertResult(condition.expectedOut)(solution.results(condition).fold(_ => ???, states => states.last.out))
  }

}

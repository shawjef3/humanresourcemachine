package me.jeffshaw.humanresourcemachine

import org.scalatest.FunSuite
import scalaz._, Scalaz._

class Spec extends FunSuite {

  test("jump") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = None,
      board = Vector(),
      here = 5
    )

    val actual =
      Jump(3).run(
        Level(Set()),
        state
      )

    val expected = state.copy(here = 2).right

    assertResult(expected)(actual)
  }

  test("jumpifzero success") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = Some(0),
      board = Vector(),
      here = 5
    )

    val actual =
      JumpIfZero(3).run(
        Level(Set()),
        state
      )

    val expected = state.copy(here = 2).right

    assertResult(expected)(actual)
  }

  test("jumpifzero failure") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = Some(1),
      board = Vector(),
      here = 5
    )

    val actual =
      JumpIfZero(3).run(
        Level(Set()),
        state
      )

    val expected = state.copy(here = 6).right

    assertResult(expected)(actual)
  }

  test("jumpifzero empty hands") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = None,
      board = Vector(),
      here = 5
    )

    val actual =
      JumpIfZero(3).run(
        Level(Set()),
        state
      )

    val expected = DLeft("hands are empty")

    assertResult(expected)(actual)
  }

  test("BumpUp") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = None,
      board = Vector(Some(3)),
      here = 5
    )

    val actual =
      BumpUp(0).run(
        Level(Set()),
        state
      )

    val expected =
      state.copy(
        board = Vector(Some(4)),
        hands = Some(4),
        here = state.here + 1
      ).right

    assertResult(expected)(actual)
  }

  test("BumpDown") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = None,
      board = Vector(Some(3)),
      here = 5
    )

    val actual =
      BumpDown(0).run(
        Level(Set()),
        state
      )

    val expected =
      state.copy(
        board = Vector(Some(2)),
        hands = Some(2),
        here = state.here + 1
      ).right

    assertResult(expected)(actual)
  }

  test("Add") {
    val state = State(
      in = Vector(),
      out = Vector(),
      hands = Some(4),
      board = Vector(Some(3)),
      here = 5
    )

    val actual =
      Add(0).run(
        Level(Set()),
        state
      )

    val expected =
      state.copy(
        board = Vector(Some(3)),
        hands = Some(7),
        here = state.here + 1
      ).right

    assertResult(expected)(actual)
  }

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

    val level = Level(Set(condition))

    val solution =
      Solution(
        level = level,
        instructions = Vector(
    /* 0*/Inbox,
    /* 1*/CopyTo(0),
    /* 2*/Outbox,
    /* 3*/CopyFrom(0),
    /* 4*/JumpIfZero(1),
    /* 5*/JumpIfNegative(12),
    /* 6*/BumpDown(0),
    /* 7*/Outbox,
    /* 8*/CopyFrom(0),
    /* 9*/JumpIfZero(1),
    /*10*/Jump(4),
          // negative branch
    /*11*/CopyFrom(0),
    /*12*/BumpUp(0),
    /*13*/Outbox,
    /*14*/CopyFrom(0),
    /*15*/JumpIfZero(1),
    /*16*/Jump(12)
        )
      )

    assertResult(1)(solution.results.size)
    assertResult(condition.expectedOut)(solution.results(condition).fold(_ => ???, states => states.last.out))
  }

  test("multiplication workshop") {
    def skipEveryOther(v: Vector[Int]): Vector[Int] = {
      if (v.isEmpty) {
        Vector.empty
      } else {
        v.head +: skipEveryOther(v.drop(2))
      }
    }

    val condition = {
      val in = Vector(2,3,1,1)

      val expectedOuts =
        skipEveryOther(in).zip(skipEveryOther(in.tail)).map {
          case (h,t) => h + t
        }.reverse

      Level.Condition(
        in = in,
        expectedOut = expectedOuts
      )
    }

    val level = Level(Set(condition))

    val solution =
      Solution(
        level = level,
        instructions = Vector(
          Inbox,
          CopyTo(0),
          CopyTo(2),
          Inbox,
          CopyTo(1),
          JumpIfZero(21),
          CopyFrom(0),
          JumpIfZero(21),
          BumpDown(1),
          JumpIfZero(23),
          CopyFrom(2),
          Add(0),
          CopyTo(0),
          BumpDown(1),
          JumpIfZero(18),
          CopyFrom(2),
          Jump(11),
          CopyFrom(0),
          Outbox,
          Jump(1),
          Outbox,
          Jump(1),
          CopyFrom(0),
          Outbox,
          Jump(1)
        )
      )

    assertResult(1)(solution.results.size)
    assertResult(condition.expectedOut)(solution.results(condition).fold(x => {
      println(x._2 + " after " + x._1)
      x._1.last.out
    }, states => states.last.out))
  }

}

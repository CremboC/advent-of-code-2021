package dev.imbrasas.day03

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

case class Count(no: Int, yes: Int):
  def add(b: Boolean): Count =
    if b then copy(yes = yes + 1) else copy(no = no + 1)

  lazy val max: Boolean = yes > no

  lazy val maxString: String = if max then "1" else "0"
  lazy val minString: String = if max then "0" else "1"

object Count:
  def empty: Count = Count(0, 0)

case class State(state: Array[Count]):
  def add(
      arr: Array[Boolean]
  ): State =
    State(
      state.zipAll(arr, Count.empty, false).map { (l, r) =>
        l.add(r)
      }
    )

  lazy val minString: String =
    state.map(_.minString).mkString

  lazy val maxString: String =
    state.map(_.maxString).mkString

object State:
  def empty: State =
    State(Array.empty)

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] =
    Input
      .lines("day03.txt")
      .fold(State.empty) { (state, bits) =>
        val array =
          bits.split("").map(s => if s == "0" then false else true)

        state.add(array)
      }
      .compile
      .lastOrError
      .flatMap { state =>
        val gamma = Integer.parseInt(state.maxString, 2)
        val epsilon = Integer.parseInt(state.minString, 2)
        for
          _ <- IO.println(s"Gamma: ${state.maxString}=${gamma}")
          _ <- IO.println(s"Epsilon: ${state.minString}=${epsilon}")
          _ <- IO.println(s"Answer: ${gamma * epsilon}")
        yield ()
      }

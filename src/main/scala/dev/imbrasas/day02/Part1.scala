package dev.imbrasas.day02

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}
import fs2.io.file.{Files, Path}

object Part1 extends IOApp.Simple with Day02:
  case class State(vertical: Int, horizontal: Int)

  def run: IO[Unit] =
    input
      .fold(State(0, 0)) {
        case (state, Command(Direction.forward, step)) =>
          State(state.vertical, state.horizontal + step)
        case (state, Command(Direction.up, step)) =>
          State(state.vertical - step, state.horizontal)
        case (state, Command(Direction.down, step)) =>
          State(state.vertical + step, state.horizontal)
      }
      .compile
      .last
      .flatMap {
        case Some(State(v, h)) => IO.println(v * h)
        case None              => IO.raiseError(new Throwable("Error"))
      }

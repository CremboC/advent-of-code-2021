package dev.imbrasas.day02

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}
import dev.imbrasas.day02.Day02
import fs2.io.file.{Files, Path}

object Part2 extends IOApp.Simple with Day02:
  case class State(vertical: Int, horizontal: Int, aim: Int)

  def run: IO[Unit] =
    input
      .fold(State(0, 0, 0)) {
        case (state, Command(Direction.forward, step)) =>
          State(
            state.vertical + (state.aim * step),
            state.horizontal + step,
            state.aim
          )
        case (state, Command(Direction.up, step)) =>
          State(state.vertical, state.horizontal, state.aim - step)
        case (state, Command(Direction.down, step)) =>
          State(state.vertical, state.horizontal, state.aim + step)
      }
      .compile
      .last
      .flatMap {
        case Some(State(v, h, _)) => IO.println(v * h)
        case None                 => IO.raiseError(new Throwable("Error"))
      }

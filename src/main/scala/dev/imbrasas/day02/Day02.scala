package dev.imbrasas.day02

import cats.effect.IO
import dev.imbrasas.util.Input
import fs2.Stream

case class Command(direction: Direction, step: Int)

enum Direction:
  case forward, down, up

trait Day02:
  def input: Stream[IO, Command] =
    Input
      .lines("day02.txt")
      .map { line =>
        val Array(dir, step) = line.split(' ')
        Command(Direction.valueOf(dir), step.toInt)
      }

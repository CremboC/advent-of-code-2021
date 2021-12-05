package dev.imbrasas.day05

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import fs2.Stream
import scala.collection.immutable.Queue

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] =
    Input
      .lines("day05.txt")
      .map(Line.parse(_))
      .filter(line => line.f.x == line.t.x || line.f.y == line.t.y)
      .fold(State(Map.empty)) { (state, line) =>
        state.add(line)
      }
      .map { state =>
        state.map.foldLeft(0) { case (acc, (point, overlaps)) =>
          if overlaps >= 2 then acc + 1
          else acc
        }
      }
      .printlns
      .compile
      .drain

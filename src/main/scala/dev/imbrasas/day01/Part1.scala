package dev.imbrasas.day01

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}
import fs2.io.file.{Files, Path}

import java.nio.file.Paths

object Part1 extends IOApp.Simple:
  def run: IO[Unit] =
    Input
      .lines("day01.txt")
      .map(_.toInt)
      .zipWithPrevious
      .fold(0) {
        case (acc, (Some(prev), curr)) =>
          if curr > prev then acc + 1 else acc
        case (acc, (None, curr)) =>
          acc
      }
      .compile
      .last
      .flatMap(IO.println)





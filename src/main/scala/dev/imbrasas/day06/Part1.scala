package dev.imbrasas.day06

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import fs2.Stream
import cats.Show
import fs2.Chunk

object Part1 extends IOApp.Simple:
  val maxDay = 80
  override def run: IO[Unit] =
    Input
      .lines("day06.txt")
      .flatMap(line =>
        Stream.emits(line.split(',').flatMap(_.toIntOption).map(Fish(_)))
      )
      .through { fishes =>
        def loop(day: Int, fishes: Stream[IO, Fish]): Stream[IO, Fish] =
          if day == maxDay then fishes
          else
            fishes
              .flatMap { fish =>
                if fish.isZero then Stream.chunk(Chunk(Fish(6), Fish(8)))
                else Stream(fish.minusOne)
              }
              .through(loop(day + 1, _))

        loop(0, fishes)
      }
      .fold(0L) { (count, fish) => count + 1 }
      .printlns
      .compile
      .drain

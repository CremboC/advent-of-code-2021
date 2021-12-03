package dev.imbrasas.day03

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import fs2.Stream

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] =
    def recurse(
        idx: Int,
        maxIdx: Int,
        condition: (Int, Int) => Char,
        stream: Stream[IO, String]
    ): Stream[IO, String] =
      for
        zeros <- Stream.eval(Ref.of[IO, Int](0))
        ones <- Stream.eval(Ref.of[IO, Int](0))
        out <- stream
          .flatMap { line =>
            Stream.exec {
              line(idx) match
                case '0' =>
                  zeros.update(_ + 1)
                case '1' =>
                  ones.update(_ + 1)
            }
          }
          .onComplete(
            Stream
              .eval {
                for
                  z <- zeros.get
                  o <- ones.get
                yield (z + o, condition(z, o))
              }
              .flatMap { (count, most) =>
                if count == 1 then stream
                else if idx == maxIdx then
                  stream.filter(line => line(idx) == most)
                else
                  stream
                    .filter(line => line(idx) == most)
                    .through(recurse(idx + 1, maxIdx, condition, _))
              }
          )
      yield out

    val input =
      Input.lines("day03.txt")

    input
      .take(1)
      .flatMap { line =>
        val oxygen = input
          .through(
            recurse(0, line.length - 1, (z, o) => if z > o then '0' else '1', _)
          )

        val co2 = input
          .through(
            recurse(0, line.length - 1, (z, o) => if z > o then '1' else '0', _)
          )

        oxygen ++ co2
      }
      .compile
      .toList
      .flatMap { case List(oxygenBinary, co2Binary) =>
        val oxygen = Integer.parseInt(oxygenBinary, 2)
        val co2 = Integer.parseInt(co2Binary, 2)
        for
          _ <- IO.println(s"Oxygen: ${oxygenBinary}")
          _ <- IO.println(s"CO2: ${co2Binary}")
          _ <- IO.println(s"Answer: ${oxygen * co2}")
        yield ()
      }

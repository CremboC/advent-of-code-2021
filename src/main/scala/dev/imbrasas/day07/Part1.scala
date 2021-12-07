package dev.imbrasas.day07

import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] =
    async[IO] {
      val input = Input.line("day07.txt").await
      val crabs = input.split(',').flatMap(_.toIntOption).toList

      val minPos = crabs.min
      val maxPos = crabs.max

      val answer = (minPos to maxPos).map { moveTo =>
        crabs.map { pos =>
          moveTo.max(pos) - moveTo.min(pos)
        }.sum
      }.min

      IO.println(answer).await
    }

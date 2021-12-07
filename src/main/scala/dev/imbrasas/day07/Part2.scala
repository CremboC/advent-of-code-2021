package dev.imbrasas.day07

import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec

object Part2 extends IOApp.Simple:
  extension (i: Int)
    def factorial: Int =
      @tailrec def loop(n: Int, acc: Int): Int =
        if n == 0 then acc
        else loop(n - 1, n + acc)

      loop(i, 0)

  override def run: IO[Unit] =
    async[IO] {
      val input = Input.line("day07.txt").await
      val crabs = input.split(',').flatMap(_.toIntOption).toList

      val minPos = crabs.min
      val maxPos = crabs.max

      val answer = (minPos to maxPos).map { moveTo =>
        crabs.map { pos =>
          (moveTo.max(pos) - moveTo.min(pos)).factorial
        }.sum
      }.min

      IO.println(answer).await
    }

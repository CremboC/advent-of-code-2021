package dev.imbrasas.day09

import cats.Eval
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import scala.annotation.tailrec

object Day09 extends IOApp.Simple:
  override def run: IO[Unit] =
    async[IO] {
      val input =
        Input
          .lines("day09.txt")
          .map(_.split("").map(_.toInt).toVector)
          .compile
          .toVector
          .await

      def at(i: Int, j: Int): Option[Int] =
        input.get(i).flatMap(_.get(j))

      @tailrec
      def loopRow(
          row: Vector[Int],
          i: Int,
          j: Int,
          acc: Map[(Int, Int), Int]
      ): Map[(Int, Int), Int] =
        row match
          case head +: rest =>
            val north = at(i - 1, j)
            val south = at(i + 1, j)
            val east = at(i, j + 1)
            val west = at(i, j - 1)

            val isLowPoint =
              List(north, south, east, west).flatten.forall(i => head < i)
            val acc_ = if isLowPoint then acc.updated((i, j), head) else acc

            loopRow(rest, i, j + 1, acc_)
          case _ =>
            acc

      @tailrec
      def loopColumn(
          columns: Vector[Vector[Int]],
          i: Int,
          acc: Map[(Int, Int), Int]
      ): Map[(Int, Int), Int] =
        columns match
          case head +: rest =>
            loopColumn(rest, i + 1, loopRow(head, i, 0, acc))
          case _ => acc

      val dangerMap = loopColumn(input, 0, Map.empty)

      val part1 = dangerMap.toList
        .sortBy(_._1)
        .foldMap((_, h) => h + 1)

      IO.println(part1).await

      def basinSize(start: Int, i: Int, j: Int): Int =
        def loop(previous: Int, i: Int, j: Int): Set[(Int, Int)] =
          List((i - 1, j), (i + 1, j), (i, j + 1), (i, j - 1))
            .traverse { (i_, j_) =>
              at(i_, j_) match
                case Some(h) if h > previous && h != 9 =>
                  Eval.always(loop(h, i_, j_) + ((i_, j_)))
                case _ =>
                  Eval.now(Set.empty)
            }
            .map(_.foldLeft(Set.empty[(Int, Int)])(_ ++ _))
            .value

        loop(start, i, j).size

      val part2 = dangerMap.toList
        .sortBy(_._1)
        .map { case ((i, j), h) =>
          basinSize(h, i, j) + 1
        }
        .sorted(Ordering[Int].reverse)
        .take(3)
        .foldLeft(1)(_ * _)

      IO.println(part2).await
    }

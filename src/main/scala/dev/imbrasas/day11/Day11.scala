package dev.imbrasas.day11

import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Grid(
    maxI: Int,
    maxJ: Int,
    g: Vector[Vector[Int]],
    flashes: Int = 0
):
  def at(i: Int, j: Int): Option[Int] =
    g.get(i).flatMap(_.get(j))

  def set(i: Int, j: Int, v: Int): Grid =
    copy(g = g.updated(i, g(i).updated(j, v)))

  def incrementAt(i: Int, j: Int): Grid =
    at(i, j).map(x => set(i, j, x + 1)).getOrElse(this)

  def incrementOne: Grid =
    copy(g = g.map(_.map(_ + 1)))

  def flashing: Queue[(Int, Int)] =
    g.mapWithIndex { (row, i) =>
      row.mapWithIndex { (el, j) =>
        if el > 9 then Some((i, j)) else None
      }.flatten
    }.flatten
      .to(Queue)

  def reset: Grid =
    var flashed_ = 0
    copy(
      g = g.mapWithIndex { (row, i) =>
        row.mapWithIndex { (el, j) =>
          if el > 9 then
            flashed_ += 1
            0
          else el
        }
      },
      flashes = flashes + flashed_
    )

  def allZeroes: Boolean = g.flatMap(_.filter(_ == 0)).length === (maxI * maxJ)

  def incrementAdjacent(i: Int, j: Int): Grid =
    inline def around(i: Int, j: Int): List[(Int, Int)] =
      (i - 1 to i + 1)
        .flatMap(i => (j - 1 to j + 1).map(j => (i, j)))
        .filter(_ != (i, j))
        .toList

    around(i, j).foldLeft(this) { case (acc, (i, j)) =>
      acc.incrementAt(i, j)
    }

  override def toString: String =
    g.map(_.mkString(".")).mkString("\n") + s"\n"

object Day11 extends IOApp.Simple:
  extension [A](set: Set[A]) def containsNot(a: A): Boolean = !set.contains(a)

  override def run: IO[Unit] =
    async[IO] {
      val input = Input
        .lines("day11.txt")
        .map(_.split("").map(_.toInt).toVector)
        .compile
        .toVector
        .map { vectors =>
          Grid(vectors.length, vectors.head.length, vectors)
        }
        .await

      val maxSteps = 200

      IO.println(input).await

      def microstep(
          grid: Grid,
          inc: Queue[(Int, Int)] = Queue.empty,
          flashed: Set[(Int, Int)] = Set.empty
      ): Grid =
        if inc.isEmpty then grid
        else
          val grid_ = inc.foldLeft(grid) { case (acc, (i, j)) =>
            acc.incrementAdjacent(i, j)
          }
          val flashed_ = flashed ++ inc.toSet
          val flashing = grid_.flashing.filterNot(flashed_.contains(_))

          microstep(grid_, flashing, flashed_)

      def part1(s: Int, grid: Grid): Grid =
        if s == maxSteps then grid
        else
          val grid_ = grid.incrementOne
          val flashing = grid_.flashing

          val grid__ =
            microstep(grid_, flashing, Set.empty).reset

          part1(s + 1, grid__)

      val answer = part1(0, input)
      IO.println(answer.flashes).await

      def part2(s: Int, grid: Grid): Int =
        val grid_ = grid.incrementOne
        val flashing = grid_.flashing

        val grid__ =
          microstep(grid_, flashing, Set.empty).reset

        if grid__.allZeroes then s
        else part2(s + 1, grid__)

      val answer2 = part2(1, input)
      IO.println(answer2).await
    }

package dev.imbrasas.day13

import cats.Show
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Point(x: Int, y: Int)

case class Grid(grid: Vector[Vector[Boolean]]):
  def set(x: Int, y: Int, v: Boolean): Grid =
    copy(grid = grid.updated(y, grid(y).updated(x, v)))

  def foldAtY(y: Int): Grid =
    val top = grid.take(y)
    val bottom = grid.drop(y + 1).reverse
    val isMidpoint = grid.length / 2.0 == y
    val bottom_ =
      if isMidpoint then
        bottom.prependedAll(Vector(Vector.fill(grid.head.length)(false)))
      else bottom
    val grid_ = top.zip(bottom_).map { (l, r) =>
      l.mapWithIndex { (el, idx) =>
        r(idx) || el
      }
    }
    copy(grid_)

  def foldAtX(x: Int): Grid =
    val split = grid.map(row => (row.take(x), row.drop(x + 1).reverse))
    val grid_ = split.map { (l, r) =>
      l.mapWithIndex { (el, idx) =>
        r(idx) || el
      }
    }

    copy(grid_)

  lazy val dotCount = grid.flatMap(_.filter(identity)).length

  override def toString: String =
    grid
      .map(_.map(if _ then "#" else ".").mkString(""))
      .mkString("\n") + s"\n"

object Grid:
  def of(x: Int, y: Int): Grid =
    Grid(Vector.fill(y + 1, x + 1)(false))

object Day13 extends IOApp.Simple:

  override def run: IO[Unit] =
    async[IO] {
      val input =
        Input
          .lines("day13.txt")
          .compile
          .toList
          .await

      val dots = input.takeWhile(_ != "").map { s =>
        val Array(x, y) = s.split(",").map(_.toInt)
        Point(x, y)
      }
      val folds = input.dropWhile(_ != "").tail.map { s =>
        val Array(_, _, s_) = s.split(" ")
        val Array(dir, amm) = s_.split("=")
        (dir, amm.toInt)
      }

      val maxX = dots.maxBy(_.x).x
      val maxY = dots.maxBy(_.y).y

      val start = dots.foldLeft(Grid.of(maxX, maxY)) { (acc, point) =>
        acc.set(point.x, point.y, true)
      }

      val part1 = folds.head match
        case ("x", n) => start.foldAtX(n)
        case ("y", n) => start.foldAtY(n)
      IO.println(part1.dotCount).await

      val s = IO.realTime.await

      val part2 =
        folds.foldLeft(start) {
          case (acc, ("x", n)) => acc.foldAtX(n)
          case (acc, ("y", n)) => acc.foldAtY(n)
        }
      val e = IO.realTime.await

      IO.println(e - s).await
      IO.println(part2).await
    }

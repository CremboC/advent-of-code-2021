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
  lazy val midY = grid.length / 2.0
  def set(x: Int, y: Int, v: Boolean): Grid =
    copy(grid = grid.updated(y, grid(y).updated(x, v)))

  def foldAtY(y: Int): Grid =
    println(s"fy y=${y} l=${grid.length} mid=${grid.length / 2.0}")

    val top = grid.take(y)
    val bottom = grid.drop(y).reverse
    println(
      s"top=${top.length} bottom=${bottom.length} sum=${top.length + bottom.length}"
    )
    val grid_ = top.zip(bottom).map { (l, r) =>
      val diff = l.length - r.length
      r.zipWithIndex.foldLeft(l) { case (acc, (el, idx)) =>
        val idx_ = (idx + diff).max(0)
        acc.updated(idx_, el || acc(idx_))
      }
    }
    copy(grid_)

  def foldAtX(x: Int): Grid =
    println(s"fx x=${x} l=${grid.head.length} mid=${grid.head.length / 2.0}")
    val split = grid.map(row => (row.take(x), row.drop(x).reverse))
    println(
      s"top=${split.head._1.length} bottom=${split.head._2.length}, sum=${split.head._1.length + split.head._2.length}"
    )
    val grid_ = split.map { (l, r) =>
      val diff = l.length - r.length
      println(s"diff=${diff}")
      r.zipWithIndex.foldLeft(l) { case (acc, (el, idx)) =>
        val idx_ = (idx + diff).max(0)
        acc.updated(idx_, el || acc(idx_))
      }
    }

    copy(grid_)

  lazy val dotCount = grid.flatMap(_.filter(identity)).length

  override def toString: String =
    grid
      .map(_.map(if _ then "█" else "░").mkString(""))
      .mkString("\n") + s"\n"

object Grid:
  def of(x: Int, y: Int): Grid =
    Grid(
      (0 to y).map { y_ =>
        (0 to x).map { x_ =>
          false
        }.toVector
      }.toVector
    )

object Day13 extends IOApp.Simple:

  override def run: IO[Unit] =
    async[IO] {
      val input =
        Input
          .lines("day13_sample.txt")
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

      // IO.println(start).await
      // IO.println(start.foldAtX(4)).await
      // IO.println(start.foldAtX(8).foldAtY(8)).await
      // IO.println(start.foldAtY(2).foldAtX(2)).await

      // val part1 = folds.head match
      //   case ("x", n) => start.foldAtX(n)
      //   case ("y", n) => start.foldAtY(n)
      // IO.println(part1.dotCount).await

      val s = IO.realTime.await

      val part2 =
        folds.foldLeft(start) {
          case (acc, ("x", n)) =>
            val acc_ = acc.foldAtX(n)
            acc_
          case (acc, ("y", n)) =>
            val acc_ = acc.foldAtY(n)
            acc_
        }
      val e = IO.realTime.await

      IO.println(e - s).await
      IO.println(part2).await
    }

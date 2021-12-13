package dev.imbrasas.day11

import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec

case class Grid(g: Vector[Vector[Int]]):
  def at(i: Int, j: Int): Option[Int] =
    g.get(i).flatMap(_.get(j))

  

  

object Day11 extends IOApp.Simple:
  override def run: IO[Unit] =
    async[IO] {
      val input = Input
        .lines("day11_sample.txt")
        .map(_.split("").map(_.toInt).toVector)
        .compile
        .toVector
        .map(Grid(_))
        .await

      val maxSteps = 100

      def microstep(grid: Grid, i: Int, j: Int): Grid =

        @tailrec
        def loopRow(row: Vector[Int], i: Int, j: Int, acc: Grid): Grid =
          row match
            case head :+ rest =>
              if head == 9 then 

            case _ => acc


        @tailrec          
        def loopColumn()
        ???

      def step(s: Int, grid: Grid, flashes: Int): Int =
        if s == maxSteps then flashes
        else

          1
          ???

      val answer = step(0, input, 0)
      IO.println(answer).await
    }

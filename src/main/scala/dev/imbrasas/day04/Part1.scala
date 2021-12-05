package dev.imbrasas.day04

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import fs2.Stream
import scala.collection.immutable.Queue

object Part1 extends IOApp.Simple:
  val sizeX = 5
  val sizeY = 5

  override def run: IO[Unit] =
    Input
      .lines("day04.txt")
      .through { stream =>
        for
          line <- stream.take(1)
          numbers = line.split(',').flatMap(_.toIntOption).toList
          _ <- Stream.eval(IO.println(numbers))
          matrices <- Stream.eval(
            stream
              .drop(1)
              .groupAdjacentByLimit(sizeY)(_ != "")
              .filter((_, lst) => lst.size > 1)
              .map { case (_, input) =>
                val inputMatrix = input.toList
                  .map(_.split(' ').toList)
                  .map(
                    _.flatMap(_.trim.toIntOption).map(Either.right[Int, Int](_))
                  )

                Matrix(inputMatrix)
              }
              .compile
              .toList
          )
          solution = solve(numbers, matrices)
          _ <- Stream.eval(IO.println(solution))
        yield ()

      }
      .compile
      .drain

  def markAndFind(
      number: Int,
      matrices: List[Matrix],
      acc: Queue[Matrix]
  ): (List[Matrix], Boolean) =
    matrices match
      case matrix :: rest =>
        val matrix_ = matrix.mark(number)
        if matrix_.check then (List(matrix_), true)
        else markAndFind(number, rest, acc.appended(matrix_))
      case Nil =>
        (acc.toList, false)

  def solve(numbers: List[Int], matrices: List[Matrix]): Option[Int] =
    numbers match
      case head :: rest =>
        val (modified, found) = markAndFind(head, matrices, Queue.empty)

        if found then modified.headOption.map(_.sumUnmarked * head)
        else solve(rest, modified)
      case Nil => None
end Part1

package dev.imbrasas.day04

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import fs2.Stream
import scala.collection.immutable.Queue

object Part2 extends IOApp.Simple:
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
              .groupAdjacentByLimit(5)(_ != "")
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
          solution = solve(numbers, Queue.from(matrices), Queue.empty)
          _ <- Stream.eval(IO.println(solution))
        yield ()

      }
      .compile
      .drain

  def markAndFind(
      number: Int,
      matrices: Queue[Matrix],
      unsolved: Queue[Matrix],
      solved: Queue[Matrix]
  ): (Queue[Matrix], Queue[Matrix]) =
    matrices.dequeueOption match
      case Some((matrix, rest)) =>
        val matrix_ = matrix.mark(number)
        val isSolved = matrix_.check
        val (unsolved_, solved_) =
          if isSolved then (unsolved, solved.appended(matrix_))
          else (unsolved.appended(matrix_), solved)
        markAndFind(number, rest, unsolved_, solved_)
      case None =>
        (unsolved, solved)

  def solve(
      numbers: List[Int],
      unsolved: Queue[Matrix],
      solved: Queue[Matrix]
  ): Option[Int] =
    numbers match
      case head :: rest =>
        val (unsolved_, solved_) =
          markAndFind(head, unsolved, Queue.empty, Queue.empty)

        if unsolved_.isEmpty then solved_.lastOption.map(_.sumUnmarked * head)
        else solve(rest, unsolved_, solved_)
      case Nil => None
end Part2

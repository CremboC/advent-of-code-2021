package dev.imbrasas.day10

import cats.Eval
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}

import scala.annotation.tailrec

object Day10 extends IOApp.Simple:
  val starters = Vector('(', '{', '[', '<')
  val enders = Vector(')', '}', ']', '>')

  val scorePart1 = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val scorePart2 = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def pair(a: Char, b: Char): Boolean =
    (a, b) match
      case ('(', ')') => true
      case ('[', ']') => true
      case ('{', '}') => true
      case ('<', '>') => true
      case _          => false

  inline def startToEnd(c: Char): Char =
    enders(starters.indexOf(c))

  sealed trait ParseError
  case class UnexpectedChar(expected: Char, unexpected: Char) extends ParseError
  case class UnexpectedEnd(open: List[Char]) extends ParseError
  case class UnexpectedOpen(char: Char) extends ParseError
  case class NoOpen(char: Char) extends ParseError

  override def run: IO[Unit] =
    Input
      .lines("day10.txt")
      .map { line =>
        val symbols = line.toList

        def parse(line: List[Char]): Either[ParseError, Unit] =
          def scope(input: List[Char], open: List[Char])
              : Either[ParseError, Unit] =
            input match
              case head :: rest =>
                if starters.contains(head) then scope(rest, head :: open)
                else if enders.contains(head) then
                  open.headOption match
                    case Some(o) if pair(o, head) =>
                      scope(rest, open.tail)
                    case Some(o) =>
                      Left(UnexpectedChar(startToEnd(o), head))
                    case None =>
                      Left(NoOpen(head))
                else scope(rest, open)
              case Nil =>
                if open.isEmpty then Right(())
                else Left(UnexpectedEnd(open))

          line match
            case head :: rest if enders.contains(head) =>
              Left(UnexpectedOpen(head))
            case lst =>
              scope(lst, Nil)

        (symbols, parse(symbols))
      }
      .through { stream =>
        Stream.eval {
          async[IO] {
            val parsed = stream.compile.toList.await

            val answer1 = parsed.foldLeft(0) {
              case (acc, (_, Left(UnexpectedChar(_, c)))) =>
                scorePart1(c) + acc

              case (acc, _) =>
                acc
            }

            IO.println(answer1).await

            val answer2 = parsed.collect {
              case (_, Left(UnexpectedEnd(open))) =>
                open.map(startToEnd(_)).foldLeft(0L) { case (acc, el) =>
                  (acc * 5) + scorePart2(el)
                }
            }

            IO.println(answer2.sorted.get(answer2.length / 2)).await
          }
        }
      }
      .compile
      .drain

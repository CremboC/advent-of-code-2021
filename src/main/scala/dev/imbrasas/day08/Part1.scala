package dev.imbrasas.day08

import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

object Part1 extends IOApp.Simple:
  override def run: IO[Unit] =
    val search = Set(2, 3, 4, 7)
    Input
      .lines("day08.txt")
      .map { line =>
        val Array(input, output) = line.split('|').map(_.trim)
        val inputArray = input.split(' ')
        val outputArray = output.split(' ')
        outputArray.foldLeft(0) { (acc, el) =>
          if search.contains(el.length) then acc + 1
          else acc
        }
      }
      .foldMonoid
      .printlns
      .compile
      .drain

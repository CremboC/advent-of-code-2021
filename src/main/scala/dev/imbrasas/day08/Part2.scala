package dev.imbrasas.day08

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

object Part2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val allChars = ('a' to 'g').toSet
    Input
      .lines("day08.txt")
      .map { line =>
        val Array(input, output) = line.split('|').map(_.trim)
        val inputArray = input.split(' ').map(_.toSet)
        val outputArray = output.split(' ').map(_.toSet)

        val Some(oneS) = inputArray.find(_.size == 2)
        val Some(fourS) = inputArray.find(_.size == 4)
        val Some(sevenS) = inputArray.find(_.size == 3)
        val Some(eightS) = inputArray.find(_.size == 7)

        val twoThreeOrFive = inputArray.filter(_.size == 5)
        val zeroSixOrNine = inputArray.filter(_.size == 6)

        val aSegmentSignal = sevenS.diff(oneS).head

        val List(x, y) = sevenS.intersect(oneS).toList
        val sixS = zeroSixOrNine.filter { signal =>
          signal.contains(x) ^ signal.contains(y)
        }.head

        val (cSegmentSignal, fSegmentSignal) =
          if sixS.contains(x) then (y, x)
          else (x, y)

        val zeroOrNine = zeroSixOrNine.filterNot(_ == sixS)

        val List(x_, y_) = fourS.diff(oneS).toList
        val zeroS = zeroOrNine.filter { signal =>
          signal.contains(x_) ^ signal.contains(y_)
        }.head

        val (bSegmentSignal, dSegmentSignal) =
          if zeroS.contains(y_) then (y_, x_)
          else (x_, y_)

        val nineS = zeroOrNine.filterNot(_ == zeroS).head
        val eSegmentSignal = eightS.diff(nineS).head

        val knownSignals =
          Set(
            aSegmentSignal,
            bSegmentSignal,
            cSegmentSignal,
            dSegmentSignal,
            eSegmentSignal,
            fSegmentSignal
          )

        val gSegmentSignal = allChars.diff(knownSignals).head

        val twoS = Set(
          aSegmentSignal,
          cSegmentSignal,
          dSegmentSignal,
          eSegmentSignal,
          gSegmentSignal
        )

        val threeS = Set(
          aSegmentSignal,
          cSegmentSignal,
          dSegmentSignal,
          fSegmentSignal,
          gSegmentSignal
        )

        val fiveS = Set(
          aSegmentSignal,
          bSegmentSignal,
          dSegmentSignal,
          fSegmentSignal,
          gSegmentSignal
        )

        val signalToInt = Map[Set[Char], Int](
          zeroS -> 0,
          oneS -> 1,
          twoS -> 2,
          threeS -> 3,
          fourS -> 4,
          fiveS -> 5,
          sixS -> 6,
          sevenS -> 7,
          eightS -> 8,
          nineS -> 9
        )

        val Some(result) = outputArray.map(signalToInt(_)).mkString.toIntOption
        result
      }
      .foldMonoid
      .printlns
      .compile
      .drain

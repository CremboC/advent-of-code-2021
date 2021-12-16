package dev.imbrasas.day14

import cats.Show
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Part2 extends IOApp.Simple:
  case class Rule(pair: (Char, Char), insert: Char)

  override def run: IO[Unit] =
    async[IO] {
      val input = Input.lines("day14.txt").compile.toList.await

      val template = input.head
      val rules = input
        .drop(2)
        .map { string =>
          val Array(pair, insert) = string.split(" -> ")
          Rule((pair.head, pair.last), insert.head)
        }

      @tailrec
      def solve(
          step: Int,
          maxSteps: Int,
          pairs: Map[(Char, Char), Long],
          freq: Map[Char, Long]
      ): (Map[(Char, Char), Long], Map[Char, Long]) =
        if step == maxSteps then (pairs, freq)
        else
          val (pairs_, freq_) = rules.foldLeft(
            (Map.empty[(Char, Char), Long], freq)
          ) { case ((pairs_, freq_), rule) =>
            val (l, r) = rule.pair
            pairs.get(rule.pair) match
              case Some(pairCount) =>
                val pairs__ = pairs_
                  .updatedWith((l, rule.insert)) {
                    case Some(t) => Some(t + pairCount)
                    case None    => Some(pairCount)
                  }
                  .updatedWith((rule.insert, r)) {
                    case Some(t) => Some(t + pairCount)
                    case None    => Some(pairCount)
                  }

                (
                  pairs__,
                  freq_.updatedWith(rule.insert) {
                    case Some(t) => Some(t + pairCount)
                    case None    => Some(1L)
                  }
                )
              case None =>
                (pairs_, freq_)
          }

          solve(step + 1, maxSteps, pairs_, freq_)

      def mkPairs(string: String): Map[(Char, Char), Long] =
        string
          .sliding(2)
          .toList
          .groupMapReduce(s => (s.head, s.last))(_ => 1L)(_ + _)

      def mkFreq(string: String): Map[Char, Long] =
        string.groupMapReduce(identity)(_ => 1L)(_ + _)

      val pairs = mkPairs(template)
      val freq = mkFreq(template)

      val (_, answer) = solve(0, 40, pairs, freq)
      IO.println(answer.maxBy(_._2)._2 - answer.minBy(_._2)._2).await
    }

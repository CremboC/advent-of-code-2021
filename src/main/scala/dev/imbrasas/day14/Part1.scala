package dev.imbrasas.day14

import cats.Show
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Rule(pair: String, insert: Char)

object Part1 extends IOApp.Simple:

  extension (s: String)
    def allIndexOff(str: String): Queue[Int] =
      val length = s.length
      var i = 1
      var acc = Queue.newBuilder[Int]
      while i < length do
        val substr = s.substring(i - 1, i + 1)
        if substr == str then acc.addOne(i - 1)
        i += 1

      acc.result

  override def run: IO[Unit] =
    async[IO] {
      val input = Input.lines("day14_sample.txt").compile.toList.await

      val template = input.head
      val rules = input
        .drop(2)
        .map { string =>
          val Array(pair, insert) = string.split(" -> ")
          Rule(pair, insert.head)
        }

      val maxSteps = 40

      @tailrec
      def solve(step: Int, template: String, rules: List[Rule]): String =
        println((step, template.length))
        if step == maxSteps then template
        else
          val template_ = rules
            .flatMap { rule =>
              val idx = template.allIndexOff(rule.pair)
              idx.map(i => (i + 1, rule))
            }
            .sortBy((idx, s) => idx)(Ordering[Int].reverse)
            .foldLeft(StringBuilder(template)) { case (acc, (idx, rule)) =>
              acc.insert(idx, rule.insert)
            }
            .toString
          solve(step + 1, template_, rules)

      val part1Template = solve(0, template, rules)
      val map = part1Template.groupMapReduce(identity)(_ => 1L)(_ + _)
      IO.println(map.maxBy(_._2)._2 - map.minBy(_._2)._2).await
    }

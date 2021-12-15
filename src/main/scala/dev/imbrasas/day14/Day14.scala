package dev.imbrasas.day14

import cats.Show
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Rule(pair: String, insert: String)

object Day14 extends IOApp.Simple:

  override def run: IO[Unit] =
    async[IO] {
      val input =
        Input
          .lines("day14_sample.txt")
          .compile
          .toList
          .await

      val template = input.head
      val rules = input
        .drop(2)
        .map { string =>
          val Array(pair, insert) = string.split(" -> ")
          (pair, insert.head)
        }
        .toMap

      val maxSteps = 1

      @tailrec
      def solve(step: Int, template: String, rules: Map[String, Char]): String =
        println((step, template, template.length))
        if step == maxSteps then template
        else
          val template_ = template
            .sliding(2)
            .foldLeft(StringBuilder()) { (acc, pair) =>
              println(("sliding", acc, pair, rules.get(pair)))
              rules.get(pair) match
                case Some(insert) =>
                  println(("inserting", StringBuilder(pair).insert(1, insert)))
                  acc.append(StringBuilder(pair).insert(1, insert))
                case None =>
                  acc.append(pair)
            }
            .toString
          // .foldLeft(StringBuilder(template)) { case (acc, pair) =>
          //   val ((p1, i)) :: ((p2, j)) :: Nil = pair
          //   rules.get(s"${p1}${p2}") match
          //     case Some(i) => acc.insert(j, i)
          // }

          // val template_ = rules
          //   .mapFilter { rule =>
          //     val idx = template.indexOf(rule.pair)
          //     if idx == -1 then None
          //     else Some((idx + 1, rule))
          //   }
          //   .sortBy((idx, s) => idx)(Ordering[Int].reverse)
          //   .foldLeft(StringBuilder(template)) { case (acc, (idx, rule)) =>
          //     println(s"inserting $rule at $idx")
          //     acc.insert(idx, rule.insert)
          //   }
          //   .toString
          solve(step + 1, template_, rules)

      val part1Answer = solve(0, template, rules)
      IO.println(part1Answer).await

      // expected: NBBBCNCCNBBNBNBBCHBHHBCHB
      // actual:   NBBBCNCCNBNBBCHBHHBCB
    }

package dev.imbrasas.day12

import cats.Show
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Link(t: Node, f: Node)

case class Node(name: String):
  lazy val isSmall: Boolean = name.forall(_.isLower)
  lazy val isStart: Boolean = name == "start"
  lazy val isEnd: Boolean = name == "end"

case class Cave(nodes: Queue[Node], links: Queue[Link]):
  lazy val lookup: Map[Node, Queue[Node]] =
    links
      .flatMap { case Link(n1, n2) =>
        Queue((n1 -> n2), (n2 -> n1))
      }
      .groupMap(_._1)(_._2)
      .map((from, to) => (from, to.filterNot(x => x.isStart)))

object Cave:
  def parse(strings: List[String]): Cave =
    val (nodes, links) =
      strings.foldLeft((Set.empty[Node], Queue.empty[Link])) {
        case ((nodes, links), string) =>
          val Array(s, e) = string.split("-")
          val start = Node(s)
          val end = Node(e)
          (nodes ++ Set(start, end), links.appended(Link(start, end)))
      }

    Cave(nodes.to(Queue), links)

object Day12 extends IOApp.Simple:

  override def run: IO[Unit] =
    async[IO] {
      val input =
        Cave.parse(
          Input
            .lines("day12.txt")
            .compile
            .toList
            .await
        )

      def paths(
          cave: Cave,
          path: Queue[Node]
      ): Queue[Queue[Node]] =
        path match
          case _ :+ last =>
            val links = cave.lookup(last)
            val traversals =
              links.flatMap { node =>
                if node.isSmall && path.contains(node) then Queue.empty
                else if node.isEnd then Queue(path.appended(node))
                else paths(cave, path.appended(node))
              }

            traversals
          case _ => Queue.empty

      val part1 =
        paths(input, Queue(Node("start")))
          .filter(_.last == Node("end"))
          .length
      IO.println(part1).await

      def paths_ii(
          cave: Cave,
          double: Node,
          path: Queue[Node]
      ): Queue[Queue[Node]] =
        path match
          case _ :+ last =>
            val links = cave.lookup(last)
            val traversals =
              links.flatMap { node =>
                if node.isSmall && node != double && path.contains(node) then
                  Queue.empty
                else if node.isSmall && node == double && path
                    .filter(_ == node)
                    .length == 2
                then Queue.empty
                else if node.isEnd then Queue(path.appended(node))
                else paths_ii(cave, double, path.appended(node))
              }

            traversals
          case _ => Queue.empty

      val smallCaves = input.nodes.filter(_.isSmall)

      val part2 =
        smallCaves
          .flatMap(small =>
            paths_ii(input, small, Queue(Node("start")))
              .filter(_.last == Node("end"))
          )
          .toSet

      IO.println(part2.size).await
    }

package dev.imbrasas.day12

import cats.Show
import cats.effect.*
import cats.effect.cps._
import cats.syntax.all.*
import fs2.Stream

import dev.imbrasas.util.{Input, Resources}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Cave:
  def connects(other: Cave): Boolean =
    (this, other) match
      case (Start(e), Mid(s, _)) => s == e
      case (Mid(s, _), Start(e)) => s == e
      case (Mid(_, e), End(s))   => e == s
      case (End(s), Mid(_, e))   => e == s
      case _                     => false

case class Start(e: String) extends Cave

case class Mid(s: String, e: String) extends Cave:
  def startsWith(start: String): Boolean = start == s

case class End(s: String) extends Cave

object Cave:
  def parse(string: String): List[Cave] =
    string.split("-") match
      case Array("start", e) => List(Start(e))
      case Array(e, "start") => List(Start(e))
      case Array(s, "end")   => List(End(s))
      case Array("end", s)   => List(End(s))
      case Array(s, e)       => List(Mid(s, e), Mid(e, s))

opaque type Path = Queue[Cave]
object Path:
  extension (p: Path)
    def append(q: Cave): Path = p.appended(q)
    def prepend(q: Cave): Path = p.prepended(q)
    def last: Cave = p.last

  def apply(q: Cave): Path = Queue(q)

  implicit val show: Show[Path] =
    Show.show { p =>
      p.foldLeft("") {
        case (acc, Start(e)) =>
          s"start-$e"
        case (acc, Mid(s, e)) =>
          s"${acc}-${e}"
        case (acc, End(s)) =>
          s"$acc-end"
      }
    }

object Day12 extends IOApp.Simple:

  override def run: IO[Unit] =
    async[IO] {
      val input =
        Input
          .lines("day12_sample.txt")
          .map(Cave.parse(_))
          .compile
          .toList
          .await
          .flatten

      val starts = input.collect { case c @ Start(_) => c }
      val mids = input.collect { case c @ Mid(_, _) => c }
      val ends = input.collect { case c @ End(_) => c }

      def buildStart(s: Start, mids: List[Mid]): List[Path] =
        mids
          .filter(_.connects(s))
          .map(Path(s).append(_))

      def buildEnd(p: Path, ends: List[End]): List[Path] =
        ends
          .filter(_.connects(p.last))
          .map(p.append)

      // def list(
      //     starts: List[Start],
      //     mids: List[Mid],
      //     ends: List[End],
      //     acc: List[Path]
      // ): List[Path] =
      //   starts match
      //     case start :: starts_ =>
      //       buildStart(start, mids).map { part => }
      //       Nil
      //     case Nil => Nil

      IO.println(starts).await
      IO.println(mids).await
      IO.println(ends).await
      IO.println(starts.head.connects(mids.head)).await

      val paths = starts.flatMap { s =>
        buildStart(s, mids).flatMap { part =>
          buildEnd(part, ends)
        }
      }

      IO.println(paths.length).await

      IO.println(paths).await
    }

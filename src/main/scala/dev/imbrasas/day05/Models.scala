package dev.imbrasas.day05

import scala.collection.immutable.Queue

case class Point(x: Int, y: Int)

case class Line(f: Point, t: Point):
  lazy val isDiagonal = f.x != t.x && f.y != t.y

  lazy val enumerate: Queue[Point] =
    val Point(x1, y1) = f
    val Point(x2, y2) = t

    if isDiagonal then
      val a = (y2 - y1) / (x2 - x1)
      val b = y1 - (a * x1)

      val startX = x1.min(x2)
      val endX = x1.max(x2)

      Queue.from {
        (startX to endX).map { x_ =>
          val y_ = (a * x_) + b
          Point(x_, y_)
        }.toList
      }
    else
      val startX = x1.min(x2)
      val endX = x1.max(x2)

      val startY = y1.min(y2)
      val endY = y1.max(y2)
      Queue.from {
        (startY to endY).flatMap { y_ =>
          (startX to endX).map { x_ =>
            Point(x_, y_)
          }
        }
      }

object Line:
  def parse(string: String): Line =
    val Array(Array(x1, y1), Array(x2, y2)) =
      string.split(" -> ").map(_.split(',').flatMap(_.toIntOption))
    Line(Point(x1, y1), Point(x2, y2))

case class State(map: Map[Point, Int]):
  def add(line: Line): State =
    State {
      line.enumerate.foldLeft(map) { (acc, point) =>
        acc.updatedWith(point) {
          case Some(v) => Some(v + 1)
          case None    => Some(1)
        }
      }
    }

  def toString(maxX: Int, maxY: Int): String =
    (0 to maxY)
      .map { y =>
        (0 to maxX).map { x =>
          map.get(Point(x, y)).map(_.toString).getOrElse(".")
        }.mkString
      }
      .mkString("\n")

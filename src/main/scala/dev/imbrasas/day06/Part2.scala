package dev.imbrasas.day06

import cats.effect.*
import cats.syntax.all.*
import dev.imbrasas.util.{Input, Resources}

import fs2.Stream
import cats.Show
import fs2.Chunk

case class FishState(map: Map[Fish, Long]):
  def add(fish: Fish): FishState =
    FishState {
      map.updatedWith(fish) {
        case Some(c) => Some(c + 1)
        case None    => Some(1)
      }
    }

  def day: FishState =
    val zeroes = getZeros
    val state_ =
      if zeroes > 0 then
        map
          .removed(Fish(0))
          .map((fish, count) => (fish.minusOne, count))
          .updatedWith(Fish(6)) {
            case Some(c) => Some(c + zeroes)
            case None    => Some(zeroes)
          }
          .updatedWith(Fish(8)) {
            case Some(c) => Some(c + zeroes)
            case None    => Some(zeroes)
          }
      else
        map.map { (fish, count) =>
          (fish.minusOne, count)
        }

    FishState(state_)

  def getZeros: Long = map.get(Fish(0)).getOrElse(0L)
  def countTotal: Long = map.foldLeft(0L) { case (x, (fish, count)) =>
    x + count
  }

object FishState:
  def empty: FishState = FishState(Map.empty)

object Part2 extends IOApp.Simple:
  val maxDay = 256
  override def run: IO[Unit] =
    Input
      .lines("day06.txt")
      .flatMap(line =>
        Stream.emits(line.split(',').flatMap(_.toIntOption).map(Fish(_)))
      )
      .through { fishes =>
        def loop(day: Int, fishState: Stream[IO, FishState])
            : Stream[IO, FishState] =
          if day == maxDay then fishState
          else fishState.map(_.day).through(loop(day + 1, _))

        loop(
          0,
          fishes.fold(FishState.empty)((state, fish) => state.add(fish))
        )
      }
      .map(_.countTotal)
      .printlns
      .compile
      .drain

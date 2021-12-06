package dev.imbrasas.day06

import cats.Show

opaque type Fish = Int
object Fish:
  extension (f: Fish)
    def minusOne: Fish = f - 1
    def isZero: Boolean = f == 0

  def apply(i: Int): Fish = i
  def create: Fish = 8

  implicit val show: Show[Fish] = Show.fromToString

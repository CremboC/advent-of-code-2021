package dev.imbrasas.util

import cats.effect.IO
import fs2.Stream
import fs2.io.file.Files

object Input:
  def lines(file: String): Stream[IO, String] =
    Files[IO]
      .readAll(Resources.fs2Path(file))
      .through(fs2.text.utf8.decode[IO])
      .through(fs2.text.lines[IO])

  def line(file: String): IO[String] =
    Files[IO]
      .readAll(Resources.fs2Path(file))
      .through(fs2.text.utf8.decode[IO])
      .through(fs2.text.lines[IO])
      .compile
      .lastOrError

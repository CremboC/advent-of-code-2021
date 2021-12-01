package dev.imbrasas.util

import fs2.io.file.Path

import java.nio.file.Paths

object Resources:
  def fs2Path(fileName: String): Path =
    Path.fromNioPath(Paths.get(this.getClass.getResource(s"/$fileName").toURI))

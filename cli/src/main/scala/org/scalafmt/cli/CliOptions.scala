package org.scalafmt.cli

import java.io.File

import org.scalafmt.config.ScalafmtConfig

object CliOptions {
  val default = CliOptions()
}

case class CliOptions(
    files: Seq[File] = Nil,
    exclude: Seq[File] = Nil,
    inPlace: Boolean = false,
    testing: Boolean = false,
    debug: Boolean = false,
    sbtFiles: Boolean = true,
    style: ScalafmtConfig = ScalafmtConfig.default,
    range: Set[Range] = Set.empty[Range],
    migrate: Option[File] = None,
    assumeFilename: String = "foobar.scala" // used when read from stdin
) {
  require(!(inPlace && testing), "inPlace and testing can't both be true")
}

package org.scalafmt.cli

import scala.io.Source

import java.io.File

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.util.FileOps

sealed abstract class InputMethod {
  def isSbt(options: CliOptions) =
    options.sbtFiles && filename.endsWith(".sbt")
  def readInput: String
  def filename: String
  def write(formatted: String, original: String, options: CliOptions): Unit
}

object InputMethod {

  case class StdinCode(assumedFilename: String) extends InputMethod {
    override def filename = assumedFilename
    override def readInput: String =
      Source.fromInputStream(System.in).getLines().mkString("\n")
    override def write(code: String,
                       original: String,
                       options: CliOptions): Unit = {
      options.common.out.println(code)
    }
  }
  case class FileContents(filename: String) extends InputMethod {
    def readInput: String = FileOps.readFile(filename)
    override def write(formatted: String,
                       original: String,
                       options: CliOptions): Unit = {
      {
        val codeChanged = formatted != original
        if (options.testing && codeChanged) {
          throw MisformattedFile(new File(filename))
        } else if (options.inPlace) {
          if (codeChanged) FileOps.writeFile(filename, formatted)
          else Unit
        } else {
          options.common.out.println(formatted)
        }
      }
    }
  }
}

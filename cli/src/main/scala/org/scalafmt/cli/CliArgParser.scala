package org.scalafmt.cli

import java.io.File
import java.util.Date

import org.scalafmt.Versions
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.hocon.Hocon2Class
import org.scalafmt.util.BuildTime
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitCommit
import scopt.OptionParser

object CliArgParser {
  @GitCommit val gitCommit: String = ???
  @BuildTime val buildTimeMs: Long = ???
  val usageExamples =
    """
      |// get help
      |scalafmt --help
      |
      |// print formatted contents of file to stdout.
      |scalafmt -f Code.scala
      |
      |// write formatted contents to file.
      |scalafmt -i -f Code1.scala,Code2.scala
      |
      |// format with predefined custom style
      |scalafmt --config "style=defaultWithAlign" -f Code.scala
      |
      |// read style options from a configuration file
      |$ cat .scalafmt.conf
      |maxColumn = 120
      |docstrings = JavaDoc
      |$ scalafmt --config .scalafmt -i -f Code.scala
      |
      |// format all files in current directory, write new contents to each file.
      |scalafmt -i -f .
      |
      |// read scala code from stdin and print formatted contents to stdout.
      |scalafmt
    """.stripMargin

  val scoptParser: OptionParser[CliOptions] =
    new scopt.OptionParser[CliOptions]("scalafmt") {

      def printAndExit(inludeUsage: Boolean)(ignore: Unit,
                                             c: CliOptions): CliOptions = {
        if (inludeUsage) showUsage
        else showHeader
        sys.exit
        c
      }

      def readConfigFromFile(file: String, c: CliOptions): CliOptions = {
        val contents =
          if (file.startsWith("\""))
            file.stripPrefix("\"").stripSuffix("\"")
          else FileOps.readFile(file)
        Hocon2Class
          .gimmeClass[ScalafmtConfig](contents, c.style.reader, None) match {
          case Right(style) => c.copy(style = style)
          case Left(e) => throw e
        }
      }

      head("scalafmt", Versions.nightly)
      opt[Seq[File]]('f', "files")
        .action((files, c) => c.copy(files = files))
        .text(
          "can be directory, in which case all *.scala files are formatted. " +
            "If not provided, reads from stdin.")
      opt[Seq[File]]('e', "exclude").action((exclude, c) =>
        c.copy(exclude = exclude)) text "can be directory, in which case all *.scala files are ignored when formatting."
      opt[String]('c', "config")
        .action(readConfigFromFile)
        .text(
          "read style flags, see \"Style configuration option\", from this" +
            " config file. The file can contain comments starting with //")
      opt[File]("migrate2hocon")
        .action((file, c) => c.copy(migrate = Some(file)))
        .text("""migrate .scalafmt CLI style configuration to hocon style configuration in .scalafmt.conf""")
      opt[Unit]('i', "in-place")
        .action((_, c) => c.copy(inPlace = true))
        .text("write output to file, does nothing if file is not specified")
      opt[Unit]("test")
        .action((_, c) => c.copy(testing = true))
        .text("test for mis-formatted code, exits with status 1 on failure.")
      opt[Unit]("debug")
        .action((_, c) => c.copy(debug = true))
        .text("print out debug information")
      opt[Unit]("statement")
        .action((_, c) =>
          c.copy(style = c.style.copy(runner =
            c.style.runner.copy(parser = scala.meta.parsers.Parse.parseStat))))
        .text("parse the input as a statement instead of compilation unit")
      opt[Unit]('v', "version")
        .action(printAndExit(inludeUsage = false))
        .text("print version ")
      opt[Unit]("build-info")
        .action({
          case (_, c) =>
            println(buildInfo)
            sys.exit
        })
        .text("prints build information")
      opt[Unit]('h', "help")
        .action(printAndExit(inludeUsage = true))
        .text("prints this usage text")
      opt[(Int, Int)]("range")
        .hidden()
        .action({
          case ((from, to), c) =>
            val offset = if (from == to) 0 else -1
            c.copy(range = c.range + Range(from - 1, to + offset))
        })
        .text("(experimental) only format line range from=to")
      opt[Boolean]("formatSbtFiles")
        .action((b, c) => c.copy(sbtFiles = b))
        .text(s"If true, formats .sbt files as well.")

      note(s"""
              |Examples:
              |
              |$usageExamples
              |
              |Please file bugs to https://github.com/olafurpg/scalafmt/issues
      """.stripMargin)
    }
  def buildInfo =
    s"""build commit: $gitCommit
       |build time: ${new Date(buildTimeMs)}""".stripMargin
}

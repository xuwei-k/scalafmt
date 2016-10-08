package org.scalafmt.cli

import scala.meta.Dialect
import scala.meta.dialects.Sbt0137
import scala.util.matching.Regex

import java.io.File
import java.io.OutputStreamWriter
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.Error.UnableToParseCliOptions
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.ProjectFiles
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitOps
import org.scalafmt.util.logger

object Cli {

  case class DebugError(filename: String, error: Throwable)

  def mkRegexp(filters: Seq[String]): Regex =
    filters.mkString("(", "|", ")").r

  def getFilesFromProject(projectFiles: ProjectFiles): Seq[String] = {
    import projectFiles._
    val include = mkRegexp(includeFilter)
    val exclude = mkRegexp(excludeFilter)

    def matches(path: String): Boolean =
      include.findFirstIn(path).isDefined &&
        exclude.findFirstIn(path).isEmpty

    val gitFiles = if (git) GitOps.lsTree else Nil
    (files ++ gitFiles).filter(matches)
  }

  def getFiles(config: CliOptions): Seq[String] = {
    if (config.files.nonEmpty) {
      config.files.flatMap { file =>
        FileOps.listFiles(file, config.exclude.toSet)
      }
    } else getFilesFromProject(config.style.project)
  }

  def getInputMethods(config: CliOptions): Seq[InputMethod] = {
    if (config.files.isEmpty && !config.style.project.git) {
      Seq(InputMethod.StdinCode(config.assumeFilename))
    } else {
      getFiles(config)
        .withFilter(
          x => x.endsWith(".scala") || (config.sbtFiles && x.endsWith(".sbt"))
        )
        .map(InputMethod.FileContents.apply)
    }
  }

  def handleFile(inputMethod: InputMethod, options: CliOptions): Unit = {
    val input = inputMethod.readInput
    val formatResult =
      Scalafmt.format(input, options.style, options.range)
    formatResult match {
      case Formatted.Success(formatted) =>
        inputMethod.write(formatted, input, options)
      case Formatted.Failure(e) =>
        if (options.style.runner.fatalWarnings) {
          throw e
        } else if (!options.inPlace) {
          logger.warn(s"Error in ${inputMethod.filename}: $e")
        }
    }
  }

  def run(config: CliOptions): Unit = {
    val inputMethods = getInputMethods(config)
    val counter = new AtomicInteger()
    val workingDirectory = new File(config.common.workingDirectory)
    val sbtConfig = config.copy(
      style = config.style.copy(
        runner = config.style.runner.copy(
          dialect = Sbt0137
        )))
    val msg = "Running scalafmt..."
    val termDisplay = new TermDisplay(new OutputStreamWriter(System.out))
    if (config.inPlace) termDisplay.init()
    termDisplay.startTask(msg, workingDirectory)
    termDisplay.taskLength(msg, inputMethods.length, 0)
    inputMethods.par.foreach { inputMethod =>
      val inputConfig = if (inputMethod.isSbt(config)) sbtConfig else config
      handleFile(inputMethod, inputConfig)
      termDisplay.taskProgress(msg, counter.incrementAndGet())
    }
    termDisplay.stop()
  }

  def printErrors(options: CliOptions, errors: Seq[DebugError]): Unit = {
    if (options.debug && errors.nonEmpty) {
      val list = errors.map(x => s"${x.filename}: ${x.error}")
      logger.error(s"""Found ${errors.length} errors:
                      |${list.mkString("\n")}
                      |""".stripMargin)
    }
  }

  def getConfig(args: Array[String]): Option[CliOptions] = {
    CliArgParser.scoptParser.parse(args, CliOptions.default)
  }

  def main(args: Array[String]): Unit = {
    getConfig(args) match {
      case Some(x) if x.migrate.nonEmpty =>
        val original = FileOps.readFile(x.migrate.get)
        val modified = LegacyCli.migrate(original)
        val newFile = new File(x.migrate.get.getAbsolutePath + ".conf")
        FileOps.writeFile(newFile.getAbsolutePath, modified)
        println("Wrote migrated config to file: " + newFile.getPath)
        println(
          "NOTE. This automatic migration is a best-effort, " +
            "please file an issue if it does not work as advertised.")
        println("-------------------------")
        println(modified)
      case Some(x) => run(x)
      case None => throw UnableToParseCliOptions
    }
  }
}

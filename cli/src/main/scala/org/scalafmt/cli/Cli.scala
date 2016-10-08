package org.scalafmt.cli

import scala.io.Source
import scala.meta.Dialect
import scala.meta.dialects.Sbt0137
import scala.util.control.NonFatal
import scala.util.matching.Regex

import java.io.File
import java.util.Date
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.Error.UnableToParseCliOptions
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.Versions
import org.scalafmt.config
import org.scalafmt.config.ProjectFiles
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.hocon.Hocon2Class
import org.scalafmt.util.GitOps
import org.scalafmt.util.logger
import org.scalafmt.util.{BuildTime, FileOps, GitCommit, LoggerOps}
import scopt.OptionParser
import scopt.Read

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
object CliOptions {
  val default = CliOptions()
}

object LegacyCli {
  private def gimmeStrPairs(tokens: Seq[String]): Seq[(String, String)] = {
    tokens.map { token =>
      val splitted = token.split(";", 2)
      if (splitted.length != 2)
        throw new IllegalArgumentException("pair must contain ;")
      (splitted(0), splitted(1))
    }
  }
  def migrate(contents: String): String = {
    val regexp: Seq[String => String] = Seq(
      "--bestEffortInDeeplyNestedCode" -> "bestEffortInDeeplyNestedCode = true",
      "--scalaDocs true" -> "docstrings = ScalaDoc",
      "--indentOperators false" -> "indentOperator = spray",
      "--indentOperators true" -> "",
      "--scalaDocs false" -> "docstrings = JavaDoc",
      "--reformatComments true" -> "",
      "--reformatComments false" -> "docstrings = preserve",
      "--(\\w+) (.*)" -> "$1 = $2",
      "alignTokens" -> "align.tokens",
      "noNewlinesBeforeJsNative" -> "newlines.neverBeforeJsNative",
      "allowNewlineBeforeColonInMassiveReturnTypes" -> "newlines.sometimesBeforeColonInMethodReturnType",
      "configStyleArguments" -> "optIn.configStyleArguments",
      "alignStripMarginStrings" -> "assumeStandardLibraryStripMargin",
      "binPackArguments" -> "binPack.callSite",
      "binPackParameters" -> "binPack.defnSite",
      "binPackParentConstructors" -> "binPack.parentConstructors",
      "alignByOpenParenCallSite" -> "align.openParenCallSite",
      "alignByOpenParenDefnSite" -> "align.openParenDefnSite",
      "continuationIndentCallSite" -> "continuationIndent.callSite",
      "continuationIndentDefnSite" -> "continuationIndent.defnSite",
      "alignMixedOwners" -> "align.mixedOwners",
      "spacesInImportCurlyBraces" -> "spaces.inImportCurlyBraces",
      "spaceAfterTripleEquals" -> "spaces.afterTripleEquals",
      "spaceBeforeContextBoundColon" -> "spaces.beforeContextBoundColon"
    ).map {
      case (from, to) =>
        (x: String) =>
          x.replaceAll(from, to)
    }
    val alignR = "(align.tokens = )\"?([^#\"]*)\"?(.*)$".r
    val rewriteR = "(rewriteTokens = )\"?([^#\"]*)\"?(.*)$".r
    val custom = Seq[String => String](
      x =>
        x.lines.map {
          case rewriteR(lhs, rhs, comments) =>
            val arr = gimmeStrPairs(rhs.split(",").toSeq).map {
              case (l, r) => s"""  "$l" = "$r""""
            }.mkString("\n")
            s"""rewriteTokens: {$comments
               |$arr
               |}""".stripMargin
          case alignR(lhs, rhs, comments) =>
            val arr = gimmeStrPairs(rhs.split(",").toSeq).map {
              case (l, r) if r == ".*" => s""""$l""""
              case (l, r) => s"""{ code = "$l", owner = "$r" }"""
            }.mkString("\n  ")
            s"""|$lhs[$comments
                |  $arr
                |]""".stripMargin
          case y => y
        }.mkString("\n")
    )

    (regexp ++ custom).foldLeft(contents) {
      case (curr, f) => f(curr)
    }
  }

}

object Cli {
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

  case class DebugError(filename: String, error: Throwable)

  @GitCommit val gitCommit: String = ???
  @BuildTime val buildTimeMs: Long = ???

  def buildInfo =
    s"""build commit: $gitCommit
       |build time: ${new Date(buildTimeMs)}""".stripMargin

  lazy val scoptParser: OptionParser[CliOptions] =
    new scopt.OptionParser[CliOptions]("scalafmt") {

      def printAndExit(inludeUsage: Boolean)(ignore: Unit,
                                             c: CliOptions): CliOptions = {
        if (inludeUsage) showUsage
        else showHeader
        sys.exit
        c
      }

      head("scalafmt", Versions.nightly)
      opt[Seq[File]]('f', "files") action { (files, c) =>
        c.copy(files = files)
      } text "can be directory, in which case all *.scala files are formatted. " +
        "If not provided, reads from stdin."
      opt[Seq[File]]('e', "exclude") action { (exclude, c) =>
        c.copy(exclude = exclude)
      } text "can be directory, in which case all *.scala files are ignored when formatting."
      opt[String]('c', "config") action { (file, c) =>
        val contents =
          if (file.startsWith("\""))
            file.stripPrefix("\"").stripSuffix("\"")
          else FileOps.readFile(file)
        Hocon2Class
          .gimmeClass[ScalafmtConfig](contents, c.style.reader, None) match {
          case Right(style) => c.copy(style = style)
          case Left(e) => throw e
        }
      } text "read style flags, see \"Style configuration option\", from this" +
        " config file. The file can contain comments starting with //"
      opt[File]("migrate2hocon") action { (file, c) =>
        c.copy(migrate = Some(file))
      } text """migrate .scalafmt CLI style configuration to hocon style configuration in .scalafmt.conf"""
      opt[Unit]('i', "in-place") action { (_, c) =>
        c.copy(inPlace = true)
      } text "write output to file, does nothing if file is not specified"
      opt[Unit]("test") action { (_, c) =>
        c.copy(testing = true)
      } text "test for mis-formatted code, exits with status 1 on failure."
      opt[Unit]("debug") action { (_, c) =>
        c.copy(debug = true)
      } text "print out debug information"
      opt[Unit]("statement") action { (_, c) =>
        c.copy(
          style = c.style.copy(
            runner =
              c.style.runner.copy(parser = scala.meta.parsers.Parse.parseStat)
          )
        )
      } text "parse the input as a statement instead of compilation unit"
      opt[Unit]('v', "version") action printAndExit(inludeUsage = false) text "print version "
      opt[Unit]("build-info") action {
        case (_, c) =>
          println(buildInfo)
          sys.exit
      } text "prints build information"
      opt[Unit]('h', "help") action printAndExit(inludeUsage = true) text "prints this usage text"
      opt[(Int, Int)]("range").hidden() action {
        case ((from, to), c) =>
          val offset = if (from == to) 0 else -1
          c.copy(range = c.range + Range(from - 1, to + offset))
      } text "(experimental) only format line range from=to"
      opt[Boolean]("formatSbtFiles") action { (b, c) =>
        c.copy(sbtFiles = b)
      } text s"If true, formats .sbt files as well."

      note(s"""
              |Examples:
              |
              |$usageExamples
              |
              |Please file bugs to https://github.com/olafurpg/scalafmt/issues
      """.stripMargin)
    }
  lazy val parser = scoptParser

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

  def run(config: CliOptions): Unit = {
    val inputMethods = getInputMethods(config)
    val errorBuilder = Seq.newBuilder[DebugError]
    val counter = new AtomicInteger()
    val sbtStyle = config.style.copy(
      runner = config.style.runner.copy(
        dialect = Sbt0137
      )
    )
    inputMethods.par.foreach { inputMethod =>
      val start = System.nanoTime()
      val style = if (inputMethod.isSbt(config)) sbtStyle else config.style
      val input = inputMethod.code
      Scalafmt.format(
        inputMethod.code,
        style = style,
        range = config.range
      ) match {
        case Formatted.Success(formatted) =>
          val elapsed = TimeUnit.MILLISECONDS
            .convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
          val i = counter.incrementAndGet()
          logger.info(
            f"${i + 1}%3s/${inputMethods.length} file:${inputMethod.filename}%-50s (${elapsed}ms)")
          inputMethod.write(formatted, input, config)
        case Formatted.Failure(e) if config.debug =>
          errorBuilder += DebugError(inputMethod.filename, e)
          logger.error(s"Error in ${inputMethod.filename}")
          e.printStackTrace()
        case _ =>
      }
    }
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
    scoptParser.parse(args, CliOptions.default)
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

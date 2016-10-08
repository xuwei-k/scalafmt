package org.scalafmt.cli

import java.io.File
import java.nio.file.Files

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.config
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

class CliTest extends FunSuite with DiffAssertions {
  val unformatted = """
                      |object a    extends   App {
                      |pr(
                      |
                      |"h")
                      |}
                    """.stripMargin
  // Using maxColumn 10 just to see the CLI uses the custom style.
  val expected = """object a
                   |    extends App {
                   |  pr(
                   |    "h")
                   |}
                 """.stripMargin

  def gimmeConfig(string: String): ScalafmtConfig =
    Config.fromHocon(string).right.get

  test("scalafmt -i --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      CliOptions.default
        .copy(
          config = ScalafmtConfig.default.copy(maxColumn = 7),
          inPlace = true
        )
        .withFiles(Seq(tmpFile.toFile))
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, expected)
  }

  test("scalafmt --test --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      CliOptions.default.copy(
        config = gimmeConfig(
          s"project.files = [${tmpFile.toFile.getPath}]"
        ),
        testing = true
      )
    intercept[MisformattedFile] {
      Cli.run(formatInPlace)
    }
  }

  test("scalafmt -i ignores non-scala files") {
    val tmpFile = Files.createTempFile("prefix", "suffix")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      CliOptions.default
        .copy(inPlace = true)
        .withFiles(Seq(tmpFile.toFile))
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, unformatted)
  }

  test("handles .scala and .sbt files") {
    val dir = File.createTempFile("dir", "dir")
    dir.delete()
    dir.mkdir()
    val file1 = File.createTempFile("foo", ".scala", dir)
    val file2 = File.createTempFile("foo", ".sbt", dir)
    val original1 = """
                      |object   a {
                      |println(1)
                      |}
      """.stripMargin
    val expected1 = """
                      |object a {
                      |  println(1)
                      |}
      """.stripMargin
    val original2 = """
                      |lazy val x = project
                      |.dependsOn(core)
                      |
                      |lazy val y =    project.dependsOn(core)
      """.stripMargin
    val expected2 = """
                      |lazy val x = project.dependsOn(core)
                      |
                      |lazy val y = project.dependsOn(core)
      """.stripMargin
    FileOps.writeFile(file1.getAbsolutePath, original1)
    FileOps.writeFile(file2.getAbsolutePath, original2)
    val c = CliOptions.default
    val config = CliOptions.default.copy(
      inPlace = true,
      config = c.config.copy(
        project = c.config.project.copy(
          files = Seq(dir.getPath)
        )
      )
    )
    Cli.run(config)
    val obtained1 = FileOps.readFile(file1)
    val obtained2 = FileOps.readFile(file2)
    assertNoDiff(obtained2, expected2)
  }

  test("ignores files if told so by the configuration") {
    val dir = File.createTempFile("dir", "dir")
    dir.delete()
    dir.mkdir()
    val file1 = File.createTempFile("foo", ".scala", dir)
    val file2 = File.createTempFile("bar", ".scala", dir)
    val original1 = """
                      |object   a {
                      |println(1)
                      |}
                    """.stripMargin
    val expected1 = """
                      |object a {
                      |  println(1)
                      |}
                    """.stripMargin
    val original2 = """
                      |object   a {
                      |println(1)
                      |}
                    """.stripMargin
    val expected2 = """
                      |object   a {
                      |println(1)
                      |}
                    """.stripMargin
    FileOps.writeFile(file1.getAbsolutePath, original1)
    FileOps.writeFile(file2.getAbsolutePath, original2)
    val config = CliOptions.default
      .copy(
        inPlace = true,
        config = gimmeConfig(
          s"""
             |project.files = [${file1.getPath}]
             |project.excludeFilters = [${file2.getPath}]
        """.stripMargin
        )
      )
    Cli.run(config)
    val obtained1 = FileOps.readFile(file1)
    val obtained2 = FileOps.readFile(file2)
    assertNoDiff(obtained1, expected1)
    assertNoDiff(obtained2, expected2)
  }

  test("migrate") {
    val result = LegacyCli.migrate(
      """
        |--maxColumn 100 # comment
        |--alignTokens %;Infix,%%;Infix
        |--alignTokens "a;b,c;d,e;.*" # comment
        |--reformatComments false
        |--scalaDocs false
        |--scalaDocs true
        |--alignStripMarginStrings true
        |--binPackArguments true
        |--binPackParameters true
        |--binPackParentConstructors true
        |--configStyleArguments false
        |--noNewlinesBeforeJsNative false
        |--allowNewlineBeforeColonInMassiveReturnTypes true
        |--alignByOpenParenCallSite false
        |--alignByOpenParenDefnSite false
        |--continuationIndentCallSite 3
        |--continuationIndentDefnSite 3
        |--alignMixedOwners false
        |--binPackImportSelectors true
        |--spacesInImportCurlyBraces true
        |--spaceAfterTripleEquals true
        |--spaceBeforeContextBoundColon true
        |--unindentTopLevelOperators false
        |--bestEffortInDeeplyNestedCode
        |--rewriteTokens ⇒;=>,←;<-
      """.stripMargin
    )
    val expected =
      """
        |maxColumn = 100 # comment
        |align.tokens = [
        |  { code = "%", owner = "Infix" }
        |  { code = "%%", owner = "Infix" }
        |]
        |align.tokens = [ # comment
        |  { code = "a", owner = "b" }
        |  { code = "c", owner = "d" }
        |  "e"
        |]
        |docstrings = preserve
        |docstrings = JavaDoc
        |docstrings = ScalaDoc
        |assumeStandardLibraryStripMargin = true
        |binPack.callSite = true
        |binPack.defnSite = true
        |binPack.parentConstructors = true
        |optIn.configStyleArguments = false
        |newlines.neverBeforeJsNative = false
        |newlines.sometimesBeforeColonInMethodReturnType = true
        |align.openParenCallSite = false
        |align.openParenDefnSite = false
        |continuationIndent.callSite = 3
        |continuationIndent.defnSite = 3
        |align.mixedOwners = false
        |binPackImportSelectors = true
        |spaces.inImportCurlyBraces = true
        |spaces.afterTripleEquals = true
        |spaces.beforeContextBoundColon = true
        |unindentTopLevelOperators = false
        |bestEffortInDeeplyNestedCode = true
        |rewriteTokens: {
        |  "⇒" = "=>"
        |  "←" = "<-"
        |}
      """.stripMargin
    println(result)
    assertNoDiff(result, expected)
    val Right(_) = config.Config.fromHocon(result)
  }

  test("--config can be string") {
    val Some(obtained) = Cli.getConfig(
      Array(
        "--config",
        """"maxColumn=10""""
      ))
    assert(obtained.config.maxColumn == 10)
  }

}

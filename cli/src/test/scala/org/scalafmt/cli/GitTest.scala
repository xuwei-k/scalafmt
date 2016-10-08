package org.scalafmt.cli

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.GitOps
import org.scalatest.FunSuite

class GitTest extends FunSuite with DiffAssertions {
  import FileTestOps._

  test("--git works") {
    val input =
      createDir(
        """|/foo.scala
           |object    FormatMe {
           |  val x = 1
           |}
           |/target/foo.scala
           |object A   { }
           |/.scalafmt.conf
           |maxColumn = 2
           |project.excludeFilters = [target]
           |""".stripMargin
      )
    val expected =
      """|/.scalafmt.conf
         |maxColumn = 2
         |project.excludeFilters = [target]
         |
         |/foo.scala
         |object FormatMe {
         |  val x =
         |    1
         |}
         |
         |/target/foo.scala
         |object A   { }
         |""".stripMargin
    val fakeGitOps: GitOps = new FakeGitOps(input)
    val config = CliArgParser.scoptParser
      .parse(
        Array("--git"),
        init = CliOptions.default.copy(gitOps = fakeGitOps)
      )
      .get
    Cli.run(config)
    val obtained = dir2string(input)
    assertNoDiff(obtained, expected)
  }
}

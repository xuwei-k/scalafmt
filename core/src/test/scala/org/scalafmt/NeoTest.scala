package org.scalafmt

import scala.meta.Tree
import scala.meta.parsers.Parse

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.DiffTest
import org.scalafmt.util.HasTests
import org.scalatest.FunSuite

class NeoTest extends FunSuite with DiffAssertions {
  import org.scalafmt.util.LoggerOps._
  test("example") {
    val original =
      """|object A extends B with C {
         | val x = function(arg, arg2, arg3)
         |}
      """.stripMargin
    val runner = ScalafmtRunner.default.copy(neo = true)
    val style = ScalafmtStyle.default.copy(maxColumn = 20)
    val obtained = Scalafmt.format(original, style = style, runner = runner).get
    logger.elem("\n" + obtained)
  }

}

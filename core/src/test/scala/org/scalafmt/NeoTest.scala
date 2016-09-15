package org.scalafmt

import org.scalafmt.internal.L
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class NeoTest extends FunSuite with DiffAssertions {
  import org.scalafmt.util.LoggerOps._
  test("example") {
    val original =
      """|object A extends B with C {
         | val x = function(arg, arg2, arg3)
         | val y = function(arg, arg2, arg3)
         |}
      """.stripMargin
    val runner = ScalafmtRunner.default.copy(neo = true)
    val style = ScalafmtStyle.default.copy(maxColumn = 20)
    val obtained = Scalafmt.format(original, style = style, runner = runner).get
    logger.elem("\n" + obtained)
  }

  test("dsl") {
    val args = L.TextBlock("a")

  }

}

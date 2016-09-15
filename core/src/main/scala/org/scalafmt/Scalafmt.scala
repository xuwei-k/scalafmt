package org.scalafmt

import scala.meta.Input.stringToInput
import scala.meta.Tree
import scala.util.control.NonFatal

import org.scalafmt.Error.Incomplete
import org.scalafmt.FormatEvent.CreateFormatOps
import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.DynamicProgramming
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter

object Scalafmt {

  /**
    * Format Scala code using scalafmt.
    *
    * @param code Code string to format.
    * @param style Configuration for formatting output.
    * @param runner Configuration for how the formatting should run.
    * @param range EXPERIMENTAL. Format a subset of lines.
    * @return [[FormatResult.Success]] if successful,
    *        [[FormatResult.Failure]] otherwise. If you are OK with throwing
    *        exceptions, use [[FormatResult.Success.get]] to get back a
    *        string.
    */
  def format(code: String,
             style: ScalafmtStyle = ScalafmtStyle.default,
             runner: ScalafmtRunner = ScalafmtRunner.default,
             range: Set[Range] = Set.empty[Range]): FormatResult = {
    try {
      if (code.matches("\\s*")) FormatResult.Success("\n")
      else {
        val tree = new scala.meta.XtensionParseInputLike(code)
            .parse(stringToInput, runner.parser, runner.dialect)
            .get
        if (runner.neo) {
          val search = new DynamicProgramming(tree)
          search.format
        } else {
          val formatOps = new FormatOps(tree, style, runner)
          runner.eventCallback(CreateFormatOps(formatOps))
          val formatWriter = new FormatWriter(formatOps)
          val search = new BestFirstSearch(formatOps, range, formatWriter)
          val partial = search.getBestPath
          val formattedString = formatWriter.mkString(partial.splits)
          if (partial.reachedEOF) {
            FormatResult.Success(formattedString)
          } else {
            throw Incomplete(formattedString)
          }
        }
      }
    } catch {
      // TODO(olafur) add more fine grained errors.
      case NonFatal(e) => FormatResult.Failure(e)
    }
  }
}

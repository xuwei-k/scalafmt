package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse

import metaconfig.Reader
import org.scalafmt.FormatEvent

trait ScalafmtRunnerT {

  /**
    * The default runner formats a compilation unit and listens to no events.
    */
  val default = ScalafmtRunner(debug = false,
                               eventCallback = _ => Unit,
                               parser = scala.meta.parsers.Parse.parseSource,
                               optimizer = ScalafmtOptimizer.default,
                               maxStateVisits = 1000000,
                               scala.meta.dialects.Scala211)

  /**
    * Same as [[default]], except formats the input as a statement/expression.
    *
    * An example of how to format something other than a compilation unit.
    */
  val statement = default.copy(parser = scala.meta.parsers.Parse.parseStat)

  val sbt = default.copy(dialect = scala.meta.dialects.Sbt0137)

  val eventReader: Reader[FormatEvent => Unit] =
    Reader.instance[FormatEvent => Unit] {
      case _ =>
        Right((_: FormatEvent) => Unit)
    }
  val parseReader: Reader[MetaParser] =
    Reader.instance[MetaParser] {
      case str: String => Right(Parse.parseSource) // ???
    }
  val dialectReader: Reader[Dialect] =
    Reader.instance[Dialect] {
      case str: String => Right(Scala211) // ???
    }
}

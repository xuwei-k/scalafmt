package org.scalafmt.util

import org.scalafmt.config._

object LiteralOps {

  /** Prints integer literals with specified case
    *
    * Divides literal into three parts:
    *
    * for 0xFF123L
    *   - 0x is a hex prefix
    *   - FF123 is a body
    *   - L is a long suffix
    *
    * and for 0b1010L
    *   - 0b is a bin prefix
    *   - 1010 is a body
    *   - L is a long suffix
    *
    * Then
    *   - literals.hexPrefix or literals.binPrefix applies to prefix
    *   - literals.hexDigits applies to body (if hex) and
    *   - literals.long applies to suffix
    */
  def prettyPrintInteger(
      str: String,
  )(implicit style: ScalafmtConfig, sb: StringBuilder): Unit =
    if (str.endsWith("L") || str.endsWith("l")) {
      prettyPrintHexOrBin(str.dropRight(1))
      sb.append(style.literals.long.process(str.takeRight(1)))
    } else prettyPrintHexOrBin(str)

  def prettyPrintFloat(
      str: String,
  )(implicit style: ScalafmtConfig, sb: StringBuilder): Unit =
    prettyPrintFloatingPoint(str, 'F', 'f', style.literals.float)

  def prettyPrintDouble(
      str: String,
  )(implicit style: ScalafmtConfig, sb: StringBuilder): Unit =
    prettyPrintFloatingPoint(str, 'D', 'd', style.literals.double)

  /** Prints floating point literals with specified case
    *
    * Divides literals into two parts:
    *
    * For 1.0e-10f
    *   - 1.0e-10 is a body with scientific notation
    *   - f is a float/double suffix
    *
    * Then
    *   - literals.scientific applies to body and
    *   - literals.float/double applies to suffix
    */
  private def prettyPrintFloatingPoint(
      str: String,
      suffixUpper: Char,
      suffixLower: Char,
      suffixCase: Literals.Case,
  )(implicit style: ScalafmtConfig, sb: StringBuilder): Unit =
    if (str.last == suffixUpper || str.last == suffixLower) sb
      .append(style.literals.scientific.process(str.dropRight(1)))
      .append(suffixCase.process(str.takeRight(1)))
    else sb.append(style.literals.scientific.process(str))

  private def prettyPrintHexOrBin(
      str: String,
  )(implicit style: ScalafmtConfig, sb: StringBuilder): Unit = {
    val (prefix, body) = str.splitAt(2)
    prefix match {
      case "0x" | "0X" => sb.append(style.literals.hexPrefix.process(prefix))
          .append(style.literals.hexDigits.process(body))
      case "0b" | "0B" => sb.append(style.literals.binPrefix.process(prefix))
          .append(body)
      case _ => sb.append(str)
    }
  }
}

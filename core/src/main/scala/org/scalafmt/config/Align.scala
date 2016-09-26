package org.scalafmt.config

import metaconfig.ConfigReader
import metaconfig.Reader
import org.scalafmt.AlignToken

@ConfigReader
case class Align(
    openParenCallSite: Boolean,
    openParenDefnSite: Boolean,
    mixedOwners: Boolean,
    tokens: Set[AlignToken],
    arrowEnumeratorGenerator: Boolean,
    ifWhileOpenParen: Boolean
) {
  implicit val alignReader: Reader[AlignToken] = ScalafmtStyle.alignReader

}

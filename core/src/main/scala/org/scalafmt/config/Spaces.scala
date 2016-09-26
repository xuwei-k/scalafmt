package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class Spaces(
    beforeContextBoundColon: Boolean,
    afterTripleEquals: Boolean,
    inImportCurlyBraces: Boolean
)

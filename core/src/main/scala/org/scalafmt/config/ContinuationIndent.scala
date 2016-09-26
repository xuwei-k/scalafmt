package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class ContinuationIndent(callSite: Int, defnSite: Int)



package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class OptIn(
    configStyleArguments: Boolean,
    breakChainOnFirstMethodDot: Boolean
)

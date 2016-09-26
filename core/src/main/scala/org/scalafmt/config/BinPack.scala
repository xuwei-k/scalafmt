package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class BinPack(callSite: Boolean,
                   defnSite: Boolean,
                   parentConstructors: Boolean)

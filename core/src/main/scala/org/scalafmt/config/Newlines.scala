package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class Newlines(
    neverBeforeJsNative: Boolean,
    sometimesBeforeColonInMethodReturnType: Boolean,
    alwaysBeforeCurlyBraceLambdaParams: Boolean
)

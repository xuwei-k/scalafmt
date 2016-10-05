package org.scalafmt.util

import scala.collection.breakOut

import scala.util.Try

import java.io.File

object FileExists {
  def unapply(arg: String): Option[File] = {
    val f = new File(arg)
    if (f.isFile) Some(f)
    else None
  }
}

object GitOps {
  import sys.process._
  def lsTree: Seq[File] =
    Try {
      Seq("git", "ls-tree", "-r", "HEAD", "--name-only").!!
        .split("\n")
        .collect {
          case FileExists(f) => f
        }.toSeq
    }.getOrElse(Nil)

}

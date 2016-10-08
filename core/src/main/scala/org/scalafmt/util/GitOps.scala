package org.scalafmt.util

import scala.collection.breakOut
import scala.util.Try

import java.io.File

import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig

trait GitOps {
  def lsTree: Seq[String]
  def rootDir: Option[String]
}

class GitOpsImpl extends GitOps {
  import sys.process._

  override def lsTree: Seq[String] =
    Try {
      Seq("git", "ls-tree", "-r", "HEAD", "--name-only").!!
        .split("\n")
        .toSeq
    }.getOrElse(Nil)

  override def rootDir: Option[String] =
    Try {
      Seq("git", "rev-parse", "--show-toplevel").!!
    }.toOption

}

class FakeGitOps(root: File) extends GitOps {
  override def lsTree = FileOps.listFiles(root)
  override def rootDir = Some(root.getAbsolutePath)
}

package org.scalafmt.util

import scala.collection.breakOut
import scala.util.Try

import java.io.File

import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig

object FileExists {
  def unapply(arg: String): Option[File] = {
    val f = new File(arg)
    if (f.isFile) Some(f)
    else None
  }
}

object GitOps {
  import sys.process._
  def lsTree: Seq[String] =
    Try {
      Seq("git", "ls-tree", "-r", "HEAD", "--name-only").!!
        .split("\n")
        .toSeq
    }.getOrElse(Nil)

  def rootDir: Option[String] =
    Try {
      Seq("git", "rev-parse", "--show-toplevel").!!
    }.toOption

  def createGitRepoInDir(file: File): Unit = Try {
    require(file.isDirectory)
    val path = file.getAbsolutePath
    val gitPath = new File(file, ".git").getAbsolutePath
    val commands: Seq[Seq[String]] = Seq(
      Seq("git", s"--git-dir=$gitPath", "init"),
      Seq("git", s"--git-dir=$gitPath", s"--work-tree=$path", "add", path),
      Seq("git",
          s"--git-dir=$gitPath",
          s"--work-tree=$path",
          "commit",
          "-m",
          "\"first commit\"")
    )
    commands.foreach { cmd =>
      cmd.!!
    }
  }

}

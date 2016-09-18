package org.scalafmt.internal

import scala.meta.Tree
import scala.meta._


import org.scalafmt.FormatResult

sealed abstract class L
object L {

  trait LDef {
    def ls: Seq[L]
  }

  def stack(ls: L*) = StackBlock(ls)
  def line(ls: Seq[L]*) = LineBlock(ls.flatten)

  case class TextBlock(str: String) extends L
  case class IndentBlock(n: Int, l: L) extends L

  object IndentBlock {
    def apply(n: Int, ls: Seq[L]): Seq[IndentBlock] = ls.map(IndentBlock(n, _))
  }
  case class ChoiceBlock(ls: Seq[L]) extends L

  class CompositeFormatter(val fs: Seq[L], val sep: String) extends L

  case class LineBlock(ls: Seq[L], override val sep: String = " ")
      extends CompositeFormatter(ls, sep) {
    def +(other: L*) = LineBlock(ls ++ other)
  }
  case class StackBlock(ls: Seq[L]) extends CompositeFormatter(ls, "\n")
  case class WrapBlock(ls: Seq[L], override val sep: String = " ")
      extends CompositeFormatter(ls, sep)
}

class BlockBuilder(f: Tree => L) {
  private val b = Seq.newBuilder[L]

  def add(ls: Seq[Tree]): BlockBuilder = {
    b ++= ls.map(f)
    this
  }
  def add(l: Tree): BlockBuilder = {
    b += f(l)
    this
  }

  def add(l: L): BlockBuilder = {
    b += l
    this
  }

  def parents(ts: Seq[Ctor.Call]): BlockBuilder = {
    var first = true
    ts.foreach { t =>
      if (first) {
        b += L.TextBlock(" extends ")
        first = false
      } else {
        b += L.TextBlock(" with ")
      }
      b += f(t)
    }
    this
  }

  def args(ts: Seq[Term.Arg]) = {
    var first = true
    ts.foreach { t =>
      if (!first) {
        b += L.TextBlock(", ")
      }
      b += f(t)
      first = false
    }
    this
  }

  def indent(n: Int, t: Seq[Tree]) = {
    b += L.IndentBlock(n, L.StackBlock(t.map(f)))
    this
  }

  def add(l: String): BlockBuilder = {
    b += L.TextBlock(l)
    this
  }
  def add(l: L, ls: L*): BlockBuilder = {
    b += l
    b ++= ls
    this
  }
  def result() = b.result()
}

class DynamicProgramming(tree: Tree) {
  import L.{StackBlock => SB}
  import L.{LineBlock => LB}
  import L.{TextBlock => TB}
  import L.{IndentBlock => IB}
  def lb = new BlockBuilder(loop)

  def format: FormatResult = {
    FormatResult.Success(solve(loop(tree)))
  }

  import org.scalafmt.util.LoggerOps._

  implicit def seq2l(ls: Seq[L]): L = LB(ls)
  implicit def trees2layout(tree: Seq[Tree]): Seq[L] = tree.map(loop)
  implicit def str2tb(str: String): L = TB(str)
  implicit def tree2layout(tree: Tree): L = loop(tree)

  def loop(tree: Tree): L = {
    logger.elem(tree.getClass)
    tree match {
      case t: Source =>
        SB(t.stats)
      case t: Template =>
        logger.elem(log(t))
//        val lines: Seq[L] =
        L.StackBlock(
          Seq(
            LB(t.early ++ t.parents) + " {" +
            IB(2, t.stats.getOrElse(Nil)) +
            TB("}")
          )
        )
      case t: Defn.Val =>
        L.LineBlock(
          lb.add(t.mods).add("val ").add(t.pats).add(" = ").add(t.rhs).result()
        )
      case t: Defn.Object =>
        L.LineBlock(
          lb.add(t.mods).add("object ").add(t.name).add(t.templ).result()
        )
      case t: Term.Name =>
        L.TextBlock(t.value)
      case t: Term.Apply =>
        L.LineBlock(
          lb.add(t.fun)
            .add("(")
            .args(t.args)
            .add(
              L.TextBlock(")")
            )
            .result()
        )
      case t: Ctor.Name =>
        L.TextBlock(t.value)
      case _ => L.TextBlock("foo")
    }
  }
  import L._

  def solve(problem: L): String = {
    val sb = new StringBuilder
    var indent = 0
    def iter(l: L): Unit = l match {
      case TextBlock(x) =>
        sb.append(x)
      case LineBlock(ls, sep) =>
        ls.foreach { f =>
          iter(f)
        }
      case IndentBlock(n, x) =>
        iter(LineBlock(Seq(TextBlock(" " * n), x)))
      case comp: L.CompositeFormatter =>
        comp.fs.foreach { f =>
          iter(f)
          sb.append(comp.sep)
        }
    }
    iter(problem)
    sb.toString()
  }
}

//sealed abstract class Formatter
//object Formatter {
//  class CompositeFormatter(fs: Seq[Formatter], sep: String) extends Formatter
//  case class Vertical(fs: Seq[Formatter], sep: String)
//      extends CompositeFormatter(fs, sep)
//}
//
//trait FormatterApi {
//  def -(other: Formatter): Formatter
//  def -(other: Seq[Formatter]): Formatter
//
//  def |(other: Formatter): Formatter
//  def |(other: Seq[Formatter]): Formatter
//
//  def or(other: Formatter): Formatter
//}

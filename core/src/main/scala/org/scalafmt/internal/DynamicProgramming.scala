package org.scalafmt.internal

import scala.meta.Tree
import scala.meta._

import org.scalafmt.FormatResult

sealed abstract class L
object L {

  def stack(ls: L*) = StackBlock(ls)
  def line(ls: Seq[L]*) = LineBlock(ls.flatten)

  case class TextBlock(str: String)    extends L
  case class IndentBlock(n: Int, l: L) extends L
  case class ChoiceBlock(ls: Seq[L])   extends L

  class CompositeFormatter(val fs: Seq[L], val sep: String) extends L

  case class LineBlock(ls: Seq[L], override val sep: String = " ")
      extends CompositeFormatter(ls, sep)
  case class StackBlock(ls: Seq[L]) extends CompositeFormatter(ls, "\n")
  case class WrapBlock(ls: Seq[L], override val sep: String = " ")
      extends CompositeFormatter(ls, sep)
}

class LB(f: Tree => L) {
  private val b = Seq.newBuilder[L]

  def add(ls: Seq[Tree]): LB = {
    b ++= ls.map(f)
    this
  }
  def add(l: Tree): LB = {
    b += f(l)
    this
  }

  def add(l: L): LB = {
    b += l
    this
  }

  def parents(ts: Seq[Ctor.Call]): LB = {
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

  def add(l: String): LB = {
    b += L.TextBlock(l)
    this
  }
  def add(l: L, ls: L*): LB = {
    b += l
    b ++= ls
    this
  }
  def result() = b.result()
}

class DynamicProgramming(tree: Tree) {
  def lb = new LB(loop)

  def format: FormatResult = {
    FormatResult.Success(solve(loop(tree)))
  }

  import org.scalafmt.util.LoggerOps._

  def loop(tree: Tree): L = {
    logger.elem(tree.getClass)
    tree match {
      case t: Source =>
        L.StackBlock(t.stats.map(loop))
      case t: Template =>
        logger.elem(log(t))
        val lines: Seq[L] =
          Seq(
            L.LineBlock(lb.add(t.early).parents(t.parents).add(" {").result())
          ) ++ t.stats
            .getOrElse(Seq.empty)
            .map(x => L.IndentBlock(2, loop(x))) ++
            Seq(L.TextBlock("}"))
        L.StackBlock(lines)
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

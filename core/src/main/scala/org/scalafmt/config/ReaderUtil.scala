package org.scalafmt.config

import scala.reflect.ClassTag

import metaconfig.Reader

object ReaderUtil {
  // Poor mans coproduct reader
  def oneOf[To: ClassTag](options: sourcecode.Text[To]*): Reader[To] = {
    val m = options.map(x => x.source -> x.value).toMap
    Reader.instance[To] {
      case x: String =>
        m.get(x) match {
          case Some(y) =>
            Right(y)
          case None =>
            val available = m.keys.mkString(", ")
            val msg =
              s"Unknown input '$x'. Expected one of $available"
            Left(new IllegalArgumentException(msg))
        }
    }
  }
}

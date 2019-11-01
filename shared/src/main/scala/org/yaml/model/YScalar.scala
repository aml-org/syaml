package org.yaml.model

import java.util.Objects.{hashCode => hash}

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.SourceLocation.Unknown
import org.mulesoft.lexer.{AstToken, InputRange, SourceLocation}
import org.yaml.parser.ScalarParser
//import org.yaml.parser.{ParserResult, ScalarParser}

/**
  * A Yaml Scalar
  */
class YScalar private[yaml] (val value: Any,
                             val text: String,
                             val mark: ScalarMark = NoMark,
                             location: SourceLocation,
                             parts: IndexedSeq[YPart] = IndexedSeq.empty)
    extends YValue(location, parts) {

  override def equals(obj: Any): Boolean = obj match {
    case s: YScalar => s.value == this.value
    case n: YNodeLike =>
      n.to[YScalar] exists { s =>
        val v1 = s.value
        value == v1
      }
    case _ => false
  }

  override def hashCode(): Int = hash(value)
  def plain: Boolean           = mark.plain

  override def toString: String = mark.markText(text)
}

object YScalar {

  val Null: YScalar = new YScalar(null, "null", location = Unknown)

  def apply(value: Int): YScalar =
    new YScalar(value.asInstanceOf[Long], String.valueOf(value), location = Unknown)

  def apply(value: Any): YScalar =
    new YScalar(value, String.valueOf(value), location = Unknown)

  def apply(value: Int, sourceName: String): YScalar =
    new YScalar(value.asInstanceOf[Long], String.valueOf(value), location = SourceLocation(sourceName))

  def apply(value: Any, sourceName: String): YScalar =
    new YScalar(value, String.valueOf(value), location = SourceLocation(sourceName))

  def nonPlain(value: String, sourceName: String = "") =
    new YScalar(value, value, DoubleQuoteMark, location = SourceLocation(sourceName)) // double quoted? or create a NonPlain object?

  def fromToken(astToken: AstToken, range: InputRange, sourceName: String = "") =
    new YScalar(astToken.text,
                astToken.text,
                NoMark,
                SourceLocation(sourceName),
                Array(YNonContent(range, Array(astToken), sourceName)))

  /** Used in amf-core. Deprecate? */
  def withLocation(text: String, tag: YType, loc: SourceLocation): YScalar = {
    val r = ScalarParser.parse(text, NoMark, tag.tag, loc)
    new YScalar(r.value, text, NoMark, loc, IndexedSeq.empty)
  }
}

trait ScalarMark {
  def plain: Boolean
  def markText(text: String): String = text
}

trait QuotedMark extends ScalarMark {
  val encodeChar: Char
  override def plain: Boolean                 = false
  override def markText(text: String): String = encodeChar + text.encode + encodeChar
}

object DoubleQuoteMark extends QuotedMark {
  override val encodeChar: Char = '"'
}
object SingleQuoteMark extends QuotedMark {
  override val encodeChar: Char = '\''
}

object MultilineMark extends ScalarMark {
  override def plain: Boolean = false
}
object FoldedMark extends ScalarMark {
  override def plain: Boolean = false
}
object UnknownMark extends ScalarMark {
  override def plain: Boolean = false
}
object NoMark extends ScalarMark {
  override def plain: Boolean = true
}

object ScalarMark {
  def apply(mark: String): ScalarMark = mark match {
    case "\"" => DoubleQuoteMark
    case "'"  => SingleQuoteMark
    case "|"  => MultilineMark
    case ">"  => FoldedMark
    case ""   => NoMark
    case _    => UnknownMark
  }
}

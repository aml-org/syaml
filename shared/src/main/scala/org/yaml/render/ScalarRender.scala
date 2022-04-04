package org.yaml.render
import org.mulesoft.common.core._
import org.yaml.lexer.YamlCharRules.{isIndicator, _}
import org.yaml.model._
import org.yaml.parser.ScalarParser

object ScalarRender {

  /** Core schema is the YAML schema that supports all of our types but not timestamp type. */
  def renderScalar(text: String,
                   mustBeString: Boolean = true,
                   mark: ScalarMark = NoMark,
                   indentation: Int = 0,
                   firstLineComment: String = "",
                   isCoreSchema: Boolean = true): CharSequence = {
    analyzeScalar(text, mark, mustBeString, isCoreSchema) match {
      case NoMark          => text
      case DoubleQuoteMark => '"' + text.encode + '"'
      case SingleQuoteMark =>
        "'" + text.replace("\n", "\n\n") + "'"
      case MultilineMark   => renderAsLiteral(text, firstLineComment, indentation)
    }
  }

  private def renderAsLiteral(text: String, firstLineComment: String, indentation: Int): CharSequence = {
    val builder = new StringBuilder
    val ind     = if (indentation < 0) 2 else indentation + 2
    builder += '|'

    val l = text.length
    if (text.head == ' ') builder ++= ind.toString
    if (text(l - 1) != '\n') builder += '-'
    else if (l > 1 && text(l - 2) == '\n') builder += '+'

    builder ++= firstLineComment
    var start = 0
    var end   = 0
    do {
      end = text.indexOf('\n', start)
      val str = if (end == -1) text.substring(start) else text.substring(start, end)
      builder += '\n'
      if (str.nonEmpty) {
        for (_ <- 0 until ind) builder += ' '
        builder ++= str
      }
      start = end + 1
    } while (end != -1)
    builder
  }

  def analyzeScalar(text: String, mark: ScalarMark, mustBeString: Boolean, isCoreSchema: Boolean = true): ScalarMark = {
    if (mark == DoubleQuoteMark || mark == SingleQuoteMark) return mark
    val l = text.length
    if (l == 0)
      return if (mustBeString || !mark.plain) DoubleQuoteMark else NoMark

    if (text.head == ' ' || text.endsWith("\n\n")) return DoubleQuoteMark
    if (text == "-") return DoubleQuoteMark

    var oneLine   = true
    var allSpaces = true
    var noTabs    = true
    var flowChar  = false
    val iterator  = new ScalarIterator(text)
    do {
      iterator.current match {
        case '\n'                                 => oneLine = false
        case '\t'                                 => noTabs = false
        case '\r'                                 => return DoubleQuoteMark
        case _ if !isCPrintable(iterator.current) => return DoubleQuoteMark
        case _ if iterator.shouldQuote            => flowChar = true
        case _                                    => allSpaces = false
      }
    } while (iterator.advance)

    if (oneLine) {
      if (flowChar) DoubleQuoteMark
      else if (mark.plain && noTabs && text.last != ' ') {
        if (!mustBeString) NoMark
        else {
          val sp = ScalarParser(text)
          sp.parse()
          if (sp.yType == YType.Str || (isCoreSchema && sp.yType == YType.Timestamp)) NoMark else DoubleQuoteMark
        }
      }
      else DoubleQuoteMark
    }
    else if (allSpaces) DoubleQuoteMark
    else MultilineMark
  }
  private class ScalarIterator(text: String) {

    private var c     = 0
    var current: Char = text.head
    private val until = text.length - 1

    def isFirst: Boolean = c == 0

    def isLast: Boolean = c == until

    def advance: Boolean =
      if (isLast) false
      else {
        c = c + 1
        current = text(c)
        true
      }

    def next: Char = if (isLast) '\u0000' else text(c + 1)

    def previous: Char = if (isFirst) '\u0000' else text(c - 1)

    def shouldQuote: Boolean = {
      if (isFirst) {
        current match {
          case '?' | ':' | '-' => next.isSpaceChar || next == '\\'
          case _               => isIndicator(current)
        }
      }
      else {

        /** [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
          *|| (  An ns-char preceding “#” )
          *| ( “:” Followed by an ns-plain-safe(c)  )*/
        /*( isFlowIndicator(current) || */ //This is only valid when its flow map or seq, but i don't know when it is, and in amf always render implicits part
        // [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
        // todo: talk with Emilio how to know when it's inside a flow part
        current match {
          case '#' => previous.isSpaceChar || previous == '\\'
          case ':' => isLast || next.isSpaceChar || next == '\\'
          case _   => false
        }
      }
    }
  }

}

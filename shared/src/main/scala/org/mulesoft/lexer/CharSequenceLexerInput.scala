package org.mulesoft.lexer

import org.mulesoft.common.client.lexical.Position
import org.mulesoft.common.core.BomMark
import org.mulesoft.common.core.Chars

import java.lang.Character._
import java.lang.Integer.{MAX_VALUE => IntMax}
import org.mulesoft.lexer.CharSequenceLexerInput.InputState
import org.mulesoft.lexer.LexerInput.{EofChar, Mark}

/**
  * A LexerInput backed by a CharSequence
  */
class CharSequenceLexerInput private (val data: CharSequence,
                                      val startOffset: Int,
                                      val endOffset: Int,
                                      override val sourceName: String)
    extends LexerInput {

  /** DO NOT CHANGE THIS. THIS IS AN OBSCURE HACK TO RUN IN JDK 17 (NO JOKE) */
  private val stringData = data.toString
  private var state = InputState(offset = startOffset, nextOffset = startOffset).init(data, endOffset)

  /** The index of the character relative to the beginning of the line, as a 16 bit java character. (0 based) */
  override def column: Int = if (state.lineWithBOM) state.column - 1 else state.column

  /** The current Line number (1 based). */
  override def line: Int = state.line

  /** The absolute offset (0..n) of the current character.  */
  override def offset: Int = state.offset

  /** the triple (line, column, offset) */
  override def position: Position = state.position

  /** The current code point character in the input (or LexerInput#Eof if the EoF was reached).  */
  override def current: Int = state.current

  /** Consume and advance to the next code point.  */
  override def consume(): Unit = state.consume(data, endOffset)

  /** Consume n code points.  */
  override def consume(n: Int): Unit = for (_ <- 0 until n) state.consume(data, endOffset)

  /** We're not at the Eof */
  override def nonEof: Boolean = state.nonEof

  /**
    * Return the character `i` characters ahead of the current position, (or LexerInput#Eof if the EoF was reached).
    */
  override def lookAhead(i: Int): Int = {
    if (i == 0) return current
    val off = state.nextOffset + i - 1
    if (off < 0 || off >= endOffset) return EofChar
    val chr = data.charAt(off)
    if (i > 0) {
      if (isHighSurrogate(chr) && off + 1 < endOffset && isLowSurrogate(data.charAt(off + 1)))
        return toCodePoint(chr, data.charAt(off + 1))
      return chr
    }
    if (isLowSurrogate(chr) && off - 1 > 0 && isHighSurrogate(data.charAt(off - 1)))
      return toCodePoint(data.charAt(off - 1), chr)
    chr
  }

  /** Return the sub sequence of characters between the specified positions */
  override def subSequence(start: Int, end: Int): CharSequence = {
    if (start < startOffset || end > endOffset || end < start)
      throw new IllegalArgumentException("Invalid sub-sequence")
    data.subSequence(start, end)
  }

  /** Create a mark in the Input so you can reset the input to it later */
  override def createMark(): Mark = state.copy()

  /** Reset the input to the specified offset */
  override def reset(mark: Mark): Unit = state = mark.asInstanceOf[InputState]

  override def countSpaces(offset: Int = 0, max: Int = IntMax): Int = {
    var i = 0
    var off = state.nextOffset + offset - 1
    if (off < 0) return 0
    while (i < max && off < endOffset) {
      /** DO NOT CHANGE THIS. THIS IS AN OBSCURE HACK TO RUN IN JDK 17 (NO JOKE) */
      if (stringData.charAt(off) != ' ') return i
      i = i + 1
      off += 1
    }
    i
  }
  /** Count the number of whiteSpaces from the specified offset */
  override def countWhiteSpaces(offset: Int = 0): Int = {
    var i = 0
    var off = state.nextOffset + offset - 1
    if (off < 0) return 0
    while (off < endOffset) {
      val c = data.charAt(off)
      if (c != ' ' && c != '\t' && c != '\r') return i
      i = i + 1
      off += 1
    }
    i
  }


}

object CharSequenceLexerInput {

  def apply(data: CharSequence = "",
            startOffset: Int = 0,
            endOffset: Int = IntMax,
            sourceName: String = ""): CharSequenceLexerInput = {
    new CharSequenceLexerInput(data, startOffset, Math.min(data.length(), endOffset), sourceName)
  }

  case class InputState(var column: Int = 0,
                        var line: Int = 1,
                        var offset: Int = 0,
                        var nextOffset: Int = 0,
                        var current: Int = EofChar,
                        var lineWithBOM:Boolean = false)
      extends Mark {

    private[CharSequenceLexerInput] def nonEof             = current != EofChar
    private[CharSequenceLexerInput] def position: Position = Position(line, column, offset)

    /** Consume and advance to the next code point.  */
    private[CharSequenceLexerInput] def consume(data: CharSequence, endOffset: Int): Unit = {
      if (current == '\n') {
        column = 0
        line += 1
        lineWithBOM = false
      }
      else column += nextOffset - offset
      offset = nextOffset
      if (offset >= endOffset) {
        current = EofChar
        return
      }

      if(current.toChar.isBom)
        lineWithBOM = true
      val chr = data.charAt(offset)
      nextOffset += 1

      // Check extended code points
      if (isHighSurrogate(chr) && nextOffset < endOffset) {
        val c2 = data.charAt(nextOffset)
        if (isLowSurrogate(c2)) {
          nextOffset += 1
          current = toCodePoint(chr, c2)
          return
        }
      }
      current = chr
    }

    private[CharSequenceLexerInput] def init(data: CharSequence, endOffset: Int): InputState = {
      if (endOffset != 0) {
        column = -1
        offset = -1
        consume(data, endOffset)
      }
      this
    }
  }

}

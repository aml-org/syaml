package org.mulesoft.lexer

import org.mulesoft.common.client.lexical.Position

import java.lang.Integer.MAX_VALUE
import org.mulesoft.lexer.LexerInput.Mark
import org.yaml.lexer.YamlCharRules.isWhite

/**
  * A source of characters for a Lexer.
  */
trait LexerInput {

  /** The current code point character in the input (or LexerInput#Eof if the EoF was reached).  */
  def current: Int

  /** The absolute offset (0..n) of the current character.  */
  def offset: Int

  /** The index of the character relative to the beginning of the line, as a 16 bit java character. (0 based) */
  def column: Int

  /** The current Line number (0 based). */
  def line: Int

  /** the triple (line, column, offset) */
  def position: Position

  /** Consume and advance to the next code point.  */
  def consume(): Unit

  /** Consume n code points.  */
  def consume(n: Int): Unit

  /** Consume while the condition holds.  */
  def consumeWhile(p: Int => Boolean): Unit = while (p(current)) consume()

  /** Create a mark in the Input so you can reset the input to it later */
  def createMark(): Mark

  /** Reset the input to the specified offset */
  def reset(mark: Mark): Unit

  /**
    * Return the character `i` characters ahead of the current position, (or LexerInput#Eof if the EoF was reached).
    */
  def lookAhead(i: Int): Int

  /** Return the sub-sequence of characters between the specified positions */
  def subSequence(start: Int, end: Int): CharSequence

  /** Return the name of the source, if existent (Usually a file name, or similar).  */
  val sourceName: String = ""

  /** We're not at the Eof */
  def nonEof: Boolean

  def countSpaces(offset: Int = 0, max: Int = MAX_VALUE): Int

  /** Count the number of whiteSpaces from the specified offset */
  def countWhiteSpaces(offset: Int = 0): Int
}

object LexerInput {
  final val EofChar: Int = -1
  trait Mark
}

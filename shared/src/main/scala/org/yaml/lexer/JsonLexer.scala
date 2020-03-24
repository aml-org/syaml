package org.yaml.lexer

import org.mulesoft.lexer.LexerInput.EofChar
import org.mulesoft.lexer.{BaseLexer, CharSequenceLexerInput, LexerInput, Position}
import org.yaml.lexer.JsonLexer._
import org.yaml.lexer.YamlToken._

/**
  * Json Lexer
  */
final class JsonLexer private (input: LexerInput, positionOffset: Position = Position.Zero)
    extends BaseLexer[YamlToken](input, positionOffset) {

  /** Init must initialize the stack and the current _tokenData (may be invoking advance) */
  override def initialize(): JsonLexer = {
    emit(BeginDocument)
    advance()
    this
  }

  /**
    * Process all pending tokens. Trivial implementation just emit the EofToken
    * More complex ones can continue returning pending tokens until they emit the EofToken
    */
  override protected def processPending(): Unit = emit(EndDocument)

  override protected def findToken(chr: Int): Unit = {
    chr match {
      case '['                                                             => consumeAndEmit(BeginSequence)
      case '{'                                                             => consumeAndEmit(BeginMapping)
      case ']'                                                             => consumeAndEmit(EndSequence)
      case '}'                                                             => consumeAndEmit(EndMapping)
      case ':'                                                             => consumeAndEmit(Indicator)
      case ','                                                             => consumeAndEmit(Indicator)
      case '"'                                                             => string()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => number()
      case '\n'                                                            => consumeAndEmit(LineBreak)
      case ' ' | '\t' | '\r' =>
        consumeWhile(isWhitespace)
        emit(WhiteSpace)
      case 'n' => checkKeyword("null")
      case 't' => checkKeyword("true")
      case 'f' => checkKeyword("false")
      case _ =>
        advanceToTokens(Set('[', ']', '{', '}', ':', ','))
    }
  }

  private def advanceToTokens(tokens: Set[Int]): Unit = {
    while ((!tokens.contains(currentChar.toChar)) && nonEof) {
      consume()
    }
    emit(Error)
  }

  private def checkKeyword(str: String): Unit = {
    val l = check(str)
    if (l > 0) {
      emit(BeginScalar)
      consume(l)
      emit(Text)
      emit(EndScalar)
    }
    else
      advanceToTokens(Set(',', ':'))
  }

  private def number(): Unit = {
    emit(BeginScalar)
    consume('-')
    var valid = true
    if (!consume('0')) {
      consume()
      consumeWhile(isDigit)
    }
    else if (isDigit(currentChar)) {
      valid = false
      advanceToTokens(Set(',', ']', '}', '"', '[', '{', ':'))
    }

    if (valid) {
      if (consume('.')) {
        consumeWhile(isDigit)
      }
      if (consume('e') || consume('E')) {
        consume('+') || consume('-')
        consumeWhile(isDigit)
      }
      emit(Text)
    }
    emit(EndScalar)
  }

  private def string(): Unit = { // todo: handle breaking lines (invalid in jsons)
    var hasText          = false
    def emitText(): Unit = if (hasText) { emit(Text); hasText = false }

    emit(BeginScalar)
    consumeAndEmit(Indicator)
    while (currentChar != '"' && currentChar != EofChar) {
      if (currentChar == '\\') {
        emitText()
        emit(BeginEscape)
        consumeAndEmit(Indicator)
        if (currentChar.toChar.toUpper == 'U') consume(4)
        consumeAndEmit(MetaText)
        emit(EndEscape)
      }
      else {
        hasText = true
        consume()
      }
    }
    emitText()
    consumeAndEmit(Indicator)
    emit(EndScalar)
  }

}

object JsonLexer {
  def apply(cs: CharSequence): JsonLexer = new JsonLexer(CharSequenceLexerInput(cs))

  def apply(cs: CharSequence, sourceName: String): JsonLexer =
    new JsonLexer(CharSequenceLexerInput(cs, sourceName = sourceName))

  @deprecated("Use Position argument", "") def apply(cs: CharSequence, sourceName: String, positionOffset: (Int, Int)): JsonLexer =
    JsonLexer(cs, sourceName, Position(positionOffset._1, positionOffset._2))

  def apply(cs: CharSequence, sourceName: String, positionOffset: Position): JsonLexer =
    new JsonLexer(CharSequenceLexerInput(cs, sourceName = sourceName), positionOffset)

  private def isWhitespace(c: Int) = c == ' ' || c == '\t' || c == '\r'
  private def isDigit(c: Int)      = c >= '0' && c <= '9'

}

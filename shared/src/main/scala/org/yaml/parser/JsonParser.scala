package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer._
import org.yaml.lexer.YamlToken.{BeginDocument, _}
import org.yaml.lexer.{JsonLexer, YamlToken}
import org.yaml.model.{YTag, _}

/**
  * A Json Parser
  */
class JsonParser private[parser] (override val lexer: JsonLexer)(override implicit val eh: ParseErrorHandler)
    extends BaseParser(lexer) {

  override type B = JsonBuilder

  /** Parse the Json and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = { // i can only have one doc in json
    this.keepTokens = keepTokens
    IndexedSeq(parseDocument())
  }

  private def parseDocument(): YDocument = {
    if (consumeOrError(BeginDocument)) {
      process()
      consumeOrError(EndDocument)
    }
    val d = new YDocument(SourceLocation(lexer.sourceName), current.buildParts())
    d
  }

  def unexpected(): Unit =
    current.appendAndCheck(TokenData(Error, currentRange()), s"Unexpected '${currentText()}'")

  def expected(expected: String): Unit =
    current.appendAndCheck(
        TokenData(Error, currentRange()),
        if (currentText().isEmpty) s"Missing '$expected'" else s"Expecting '$expected' but '${currentText()}' found")

  private def expected(token: YamlToken): Boolean = {
    token match {
      case BeginScalar => expected("\"")
      case EndMapping  => expected("}")
      case EndSequence => expected("]")
      case _           => unexpected()
    }
    false
  }

  private def process(): Boolean = {
    currentToken() match {
      case BeginSequence => parseSeq()
      case BeginMapping  => parseMap()
      case BeginScalar   => parseScalar()
      case _ =>
        unexpected()
        false
    }
  }

  private def push(): Unit = {
    current = newBuilder
    stack = current :: stack
  }

  private def stackParts(part: YPart) = {
    pop()
    current.parts += part
  }

  private def pop(): Unit = {
    stack = stack.tail
    current = stack.head
  }

  private def parseMap(): Boolean = {
    push()
    val r = parseList(BeginMapping, EndMapping, MapEntryParser())
    val parts = current.buildParts()
    val v = YMap(parts, lexer.sourceName)
    stackParts(buildNode(v, YType.Map.tag))
    r
  }

  private def parseSeq(): Boolean = {
    push()
    val r = parseList(BeginSequence, EndSequence, SequenceValueParser()) // should check if i parse something? empty pop if not?
    val v = YSequence(SourceLocation(lexer.sourceName), current.buildParts())
    stackParts(buildNode(v, YType.Seq.tag))
    r
  }

  private def parseEscaped() = {
    val metaTextBuilder = new StringBuilder
    while (notCurrent(EndEscape)) {
      currentToken() match {
        case Indicator => metaTextBuilder.append(lexer.tokenString)
        case LineBreak => metaTextBuilder.clear()
        case MetaText  => metaTextBuilder.append(lexer.tokenString)
        case _         =>
      }
      consume()
    }
    metaTextBuilder.mkString.decode(ignoreErrors = true)
  }

  private def parseScalar(): Boolean = {
    if (currentOrError(BeginScalar)) {
      push()
      current.addNonContent()
      val textBuilder = new StringBuilder
      var scalarMark  = ""
      while (notCurrent(EndScalar)) {
        currentToken() match {
          case BeginEscape => textBuilder.append(parseEscaped())
          case Indicator   => scalarMark = currentText()
          case Text        => textBuilder.append(lexer.tokenText)
          case _           =>
        }
        consume()
      }
      consumeOrError(EndScalar)
      current.addNonContent()
      val tagType = if (scalarMark == DoubleQuoteMark.encodeChar.toString) YType.Str.tag else null
      val b       = new YScalar.Builder(textBuilder.toString(), tagType, scalarMark, current.buildParts(), lexer.sourceName) // always enter with begin scalar
      stackParts(buildNode(b.scalar, b.tag))
      true
    }
    else false
  }

  private def parseList(leftToken: YamlToken, rightToken: YamlToken, parser: ElementParser) = {
    assert(isCurrent(leftToken))
    consume()
    current.addNonContent()
    while (notCurrent(rightToken)) {
      parser.parse()
      if (notCurrent(rightToken)) {
        if (currentByTextOrError(Indicator, ",")) {
          consume()
          skipWhiteSpace()
          current.addNonContent()
          // These if are to get trailing commas
          if (currentToken() == rightToken) expected("value")
        }
      }
    }
    consumeOrError(rightToken)
  }

  trait ElementParser {
    def parse(): Unit
  }

  case class SequenceValueParser() extends ElementParser {
    override def parse(): Unit = {
      val r = process()
      if (!r) {
        discardIf(Error)
        advanceToByText((Indicator, Some(",")), (EndSequence, None))
      }
    }
  }

  case class MapEntryParser() extends ElementParser {

    override def parse(): Unit = {
      current.addNonContent()
      push() // i need new token for YMapEntry container
      if (parseEntry()) {
        val parts = current.buildParts()
        stackParts(YMapEntry(parts))
      }
      else {
        current.buildParts()
        pop()
      }
    }

    private def parseKey() = {
      val r = parseScalar()
      if (!r) {
        discardIf(Error)
        advanceTo(Indicator, EndMapping)
      }
      r
    }

    private def parseEntry(): Boolean = {
      val k = parseKey()
      if (k || currentByText(Indicator, ":")) {
        if (currentByTextOrError(Indicator, ":")) {
          consume()
          skipWhiteSpace()
          current.addNonContent()
        }
        k & parseValue()
      }
      else {
        advanceToByText((Indicator, Some(",")), (EndMapping, None))
        false
      }
    }

    private def parseValue(): Boolean = {
      val r = process()
      if (r) {
        current.addNonContent()
      }
      else {
        discardIf(Error)
        advanceTo(Indicator, EndMapping)
      }
      r
    }
  }

  private def currentToken(): YamlToken = {
    skipWhiteSpace()
    lexer.token
  }

  private def skipWhiteSpace(): Unit = {
    while (lexer.token == WhiteSpace || lexer.token == LineBreak) {
      current.append()
      lexer.advance()
    }
  }

  private def currentText(): String = lexer.tokenString

  private def currentRange() = lexer.tokenData.range

  private def consume() = {
    current.append()
    lexer.advance()
    true
  }

  private def discardIf(token: YamlToken): Unit = if (isCurrent(token)) discard()

  private def discard(): Unit = lexer.advance()

  private def advanceTo(tokens: YamlToken*): Unit = {
    while (!eof && !currentAnyOf(tokens: _*)) {
      consume()
    }
  }

  private def advanceToByText(tokensText: (YamlToken, Option[String])*): Unit = {
    def current(t: (YamlToken, Option[String])): Boolean = t._2 match {
      case Some(text) => currentByTextOrError(t._1, text)
      case _          => isCurrent(t._1)
    }

    while (!eof && !tokensText.exists(current)) {
      consume()
    }
  }

  private def eof() = currentToken() == EndDocument

  private def notCurrent(token: YamlToken) = currentToken() != token && !eof()

  private def currentAnyOf(tokens: YamlToken*) = tokens.contains(currentToken())

  private def isCurrent(token: YamlToken): Boolean = currentToken() == token

  private def currentByText(token: YamlToken, text: String) = isCurrent(token) && currentText() == text

  private def currentByTextOrError(token: YamlToken, text: String): Boolean = {
    if (currentByText(token, text)) true
    else {
      expected(text)
      false
    }
  }

  private def currentOrError(token: YamlToken) = if (isCurrent(token)) true else expected(token)

  private def consumeOrError(token: YamlToken): Boolean = if (currentOrError(token)) consume() else false

  private def buildNode(value: YValue, tag: YTag) = YNode(value, tag, sourceName = lexer.sourceName)

  override protected def newBuilder: JsonBuilder = new JsonBuilder

  class JsonBuilder extends Builder {
    def append(): Unit = append(lexer.tokenData, lexer.tokenString)

    def appendCustom(text: String): Unit = {
      if (keepTokens) tokens += AstToken(lexer.token, text, lexer.tokenData.range, parsingError = true)
      if (first == null) first = lexer.tokenData
    }

    def appendAndCheck(td: TD, text: String): Unit = {
      this appendCustom (td, text)
      addNonContent(td)
    }

    def addNonContent(): Unit =
      if (tokens.nonEmpty) {
        val content = new YNonContent(location(first.range.inputRange, tokens.last.range.inputRange), buildTokens())
        parts += content
        collectErrors(content)
      }

    def buildParts(): Array[YPart] = {

      addNonContent()
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray[YPart]
        parts.clear()
        r
      }
    }

    private def location(begin: InputRange, end: InputRange) =
      SourceLocation(lexer.sourceName, begin.lineFrom, begin.columnFrom, end.lineTo, end.columnTo)

  }
}

object JsonParser {
  def apply(s: CharSequence)(implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s))(eh)

  def obj(s: CharSequence)(implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): YObj =
    apply(s)(eh).documents()(0).obj

  def withSource(s: CharSequence, sourceName: String, positionOffset: Position = Position.Zero)(
      implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s, sourceName, positionOffset))(eh)

  @deprecated("Use Position argument", "")
  def withSourceOffset(s: CharSequence, sourceName: String, offset: (Int, Int))(
      implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): JsonParser =
    withSource(s, sourceName, Position(offset._1, offset._2))
}

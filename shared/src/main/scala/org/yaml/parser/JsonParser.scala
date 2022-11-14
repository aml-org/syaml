package org.yaml.parser

import org.mulesoft.common.client.lexical.{Position, SourceLocation}
import org.mulesoft.common.core.Strings
import org.mulesoft.lexer._
import org.yaml.lexer.YamlToken.{BeginDocument, _}
import org.yaml.lexer.{JsonLexer, YamlToken}
import org.yaml.model.{YTag, _}

import scala.collection.mutable.ArrayBuffer

/** A Json Parser
  */
class JsonParser private[parser] (val lexer: JsonLexer)(implicit val eh: ParseErrorHandler)
    extends YParser
    with DuplicateDetection {

  type TD = TokenData[YamlToken]

  lexer.initialize()

  private var keepTokens = false
  private var current    = new JsonBuilder
  private var stack      = List(current)

  /** Parse the Json and return the list of documents */
  def documents(keepTokens: Boolean = false): IndexedSeq[YDocument] = {
    this.keepTokens = keepTokens
    Array(parseDocument())
  }

  /** Parse the Json and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = { // i can only have one doc in json
    this.keepTokens = keepTokens
    IndexedSeq(parseDocument())
  }

  override def document(keepTokens: Boolean = false): YDocument = {
    this.keepTokens = keepTokens
    parseDocument()
  }

  private def parseDocument(): YDocument = {
    push()
    try {
      if (consumeOrError(BeginDocument)) {
        process()
        consumeOrError(EndDocument)
      }
    } catch {
      case se: SyamlException => eh.handle(current.location(), se)
      case other: Throwable   => throw other
    }
    val parts = current.buildParts()
    val sl    = current.location()
    pop()
    new YDocument(sl, parts)
  }

  private def reportError(msg: String): Unit = eh.handle(currentRange(), ParserException(msg))

  def unexpected(): Unit = reportError(s"Unexpected '${currentText()}'")

  def expected(expected: String): Unit =
    reportError(
        if (currentText().isEmpty) s"Missing '$expected'" else s"Expecting '$expected' but '${currentText()}' found"
    )

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
    current = new JsonBuilder
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
    val r  = parseList(BeginMapping, EndMapping, MapEntryParser())
    val sl = current.location()
    if (r) consume()

    val parts = current.buildParts()
    duplicates(parts)
    val v = YMap(sl, parts)
    stackParts(buildNode(v, YType.Map.tag))
    r
  }

  private def parseSeq(): Boolean = {
    push()
    val r = parseList(
        BeginSequence,
        EndSequence,
        SequenceValueParser()
    ) // should check if i parse something? empty pop if not?
    val sl = current.location()
    if (r) consume()
    val v = YSequence(sl, current.buildParts())
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

  private def parseScalar(): Boolean =
    if (!currentOrError(BeginScalar)) false
    else {
      push()
      current.addNonContent()
      val textBuilder = new StringBuilder
      var markChar    = ""
      while (notCurrent(EndScalar) && !isLineBreakScalarSituation) {
        currentToken() match {
          case BeginEscape => textBuilder.append(parseEscaped())
          case Indicator   => markChar = currentText()
          case Text        => textBuilder.append(lexer.tokenText)
          case _           =>
        }
        consume()
      }
      val loc = current.location()
      consumeOrError(EndScalar)
      current.addNonContent()
      val txt = textBuilder.toString()
      val sm  = ScalarMark(markChar)
      val pr  = ScalarParser.parse(txt, sm, if (sm == DoubleQuoteMark) YType.Str.tag else null, loc)
      stackParts(buildNode(new YScalar(pr.value, txt, sm, loc, current.buildParts()), pr.tag))
      true
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
    currentOrError(rightToken)
  }

  trait ElementParser {
    def parse(): Unit
  }

  case class SequenceValueParser() extends ElementParser {
    override def parse(): Unit = {
      current.addNonContent()
      val r = process()
      if (!r) {
        discardIf(Error)
        advanceToByText((Indicator, Some(",")), (EndSequence, None))
      }
      if (isLineBreakScalarSituation) saveLineBreakScalarSituation()
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
      val k         = parseKey()
      val indicator = currentByText(Indicator, ":")
      if (isLineBreakScalarSituation) {
        saveLineBreakScalarSituation()
        false
      }
      else if (k || indicator) {
        if (currentByTextOrError(Indicator, ":")) {
          consume()
          skipWhiteSpace()
          current.addNonContent()
        }
        k & ((parseValue() || indicator) || saveEntry())
      }
      else {
        advanceToByText((Indicator, Some(",")), (EndMapping, None))
        false
      }
    }

    private def saveEntry(): Boolean = {
      reportError("Expected entry")
      true
    }

    private def parseValue(): Boolean = {
      val hasValue = process()
      if (hasValue) {
        current.addNonContent()
      }
      else {
        addNullValue()
        advanceTo(Indicator, EndMapping)
      }
      hasValue
    }
  }

  private def addNullValue(): Unit = {
    discardIf(Error)
    val nullValueTokens = extractNullValueTokens()
    push()
    val location = if (nullValueTokens.nonEmpty) mergeLocations(nullValueTokens) else noTokensLocation()
    current.addNullNode(location)
  }

  private def extractNullValueTokens(): IndexedSeq[AstToken] = {
    current.parts.lastOption match {
      case Some(nonContent: YNonContent) =>
        splitNonContent(nonContent)
        nonContent.tokens.tail
      case _ =>
        val tokens = current.tokens.clone()
        current.tokens.clear()
        tokens
    }
  }

  private def splitNonContent(nonContent: YNonContent): Unit = {
    current.parts.takeRight(1)
    val indicator = nonContent.tokens.head
    current.addNonContent(IndexedSeq(indicator), indicator.location)
  }

  private def mergeLocations(tokens: IndexedSeq[AstToken]): SourceLocation = {
    SourceLocation(current.location().sourceName, tokens.head.location.from, current.location().from)
  }

  private def noTokensLocation(): SourceLocation = {
    SourceLocation(current.location().sourceName, current.location().from, current.location().from)
  }

  private def currentToken(): YamlToken = {
    skipWhiteSpace()
    val t = lexer.token
    t
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
    if (lexer.token == Error)
      eh.handle(currentRange(), LexerException(currentText()))
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

  // When a non closed scalar has a non escaped linebreak this is an error.
  // This method is to check if we are in that situation
  private def isLineBreakScalarSituation: Boolean = currentByText(Indicator, "\n")

  private def saveLineBreakScalarSituation(): Unit = {
    advanceTo(EndScalar)
    consume()
    skipWhiteSpace()
  }

  private def buildNode(value: YValue, tag: YTag) = YNode(value, tag, sourceName = lexer.sourceName)

  class JsonBuilder {

    def addNullNode(location: SourceLocation): Unit = stackParts(YNode.nullNode(location))

    var first: SourceLocation = lexer.tokenData.range
    val tokens                = new ArrayBuffer[AstToken]
    val parts                 = new ArrayBuffer[YPart]
    var value: YValue         = _

    def location(): SourceLocation = first to lexer.tokenData.range

    def append(): Unit = append(lexer.tokenData, lexer.tokenString)

    def append(td: TD, text: String = ""): Unit = {
      if (keepTokens) tokens += AstToken(td.token, text, td.range)
    }

    def addNonContent(td: TD): Unit =
      if (tokens.nonEmpty) {
        val content = new YNonContent(first to td.range, buildTokens())
        parts += content
      }

    def addNonContent(): Unit =
      if (tokens.nonEmpty) {
        val content = new YNonContent(location(), buildTokens())
        parts += content
      }

    def addNonContent(tokens: IndexedSeq[AstToken], location: SourceLocation): Unit =
      if (tokens.nonEmpty) {
        val content = new YNonContent(location, tokens)
        parts += content
      }

    def buildTokens(td: TD = null): IndexedSeq[AstToken] = {
      if (td != null) this append td
      if (tokens.isEmpty) IndexedSeq.empty
      else {
        val r = tokens.toArray[AstToken]
        tokens.clear()
        r
      }
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
  }
}

object JsonParser {
  def apply(s: CharSequence)(implicit eh: ParseErrorHandler = DefaultJsonErrorHandler()): JsonParser =
    new JsonParser(JsonLexer(s))(eh)

  def apply(s: CharSequence, maxDepth: Option[Int])(implicit eh: ParseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s, maxDepth))(eh)

  def obj(s: CharSequence)(implicit eh: ParseErrorHandler = DefaultJsonErrorHandler()): YObj =
    apply(s)(eh).document().obj

  def withSource(s: CharSequence, sourceName: String, positionOffset: Position = Position.ZERO)(
      implicit
      eh: ParseErrorHandler = DefaultJsonErrorHandler()): JsonParser =
    new JsonParser(JsonLexer(s, sourceName, positionOffset))(eh)

  def withSource(s: CharSequence, sourceName: String, positionOffset: Position, maxDepth: Option[Int])(
      implicit
      eh: ParseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s, sourceName, positionOffset, maxDepth))(eh)

  @deprecated("Use Position argument", "")
  def withSourceOffset(s: CharSequence, sourceName: String, offset: (Int, Int))(
      implicit
      eh: ParseErrorHandler = DefaultJsonErrorHandler()): JsonParser =
    withSource(s, sourceName, Position(offset._1, offset._2))
}

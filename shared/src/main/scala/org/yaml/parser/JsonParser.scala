package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{AstToken, InputRange, TokenData}
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
    if(consumeOrError(BeginDocument)){
      process()
      consumeOrError(EndDocument)
    }
    val d = YDocument(current.buildParts(), lexer.sourceName)
    d
  }

  def unexpected(): Unit =
    current.appendAndCheck(TokenData(Error, currentRange()), s"Unexpected '${currentText()}'")

  def unexpected(expected: String): Unit = {
    if (currentText().isEmpty)
      current.appendAndCheck(TokenData(Error, currentRange()), s"Missing '$expected'")
    else
      current.appendAndCheck(TokenData(Error, currentRange()), s"Expecting '$expected' but '${currentText()}' found")
  }

  def unexpected(expected: YamlToken): Unit = {
    defaultText(expected) match {
      case Some(e)=>unexpected(e)
      case _ => unexpected()
    }
  }

  private def defaultText(token:YamlToken): Option[String] = {
    token match {
      case BeginScalar => Some("\"")
      case BeginMapping => Some("{")
      case BeginSequence => Some("[")
      case EndMapping => Some("}")
      case EndSequence => Some("]")
      case _ => None

    }
  }

  private def process(): Boolean = {
    currentToken() match {
      case BeginSequence => parseSeq()
      case BeginMapping  => parseMap()
      case BeginScalar   => parseScalar()
      case _             =>
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

  private def parseMap():Boolean = {
    push()
    val r = parseList(BeginMapping, EndMapping, MapEntryParser())
    val v = YMap(current.buildParts(), lexer.sourceName)
    stackParts(buildNode(v, YType.Map.tag))
    r
  }

  private def parseSeq():Boolean = {
    push()
    val r = parseList(BeginSequence, EndSequence, SequenceValueParser()) // should check if i parse something? empty pop if not?
    val v = YSequence(current.buildParts(), lexer.sourceName)
    stackParts(buildNode(v, YType.Seq.tag))
    r
  }

  private def parseEscaped() = {
    val metaTextBuilder = new StringBuilder
    while (notCurrent(EndEscape)) {
      currentToken() match {
        case Indicator => metaTextBuilder.append(lexer.tokenString)
        case LineBreak => metaTextBuilder.clear()
        case MetaText => metaTextBuilder.append(lexer.tokenString)
        case _ =>
      }
      consume()
    }
    metaTextBuilder.mkString.decode(ignoreErrors = true)
  }

  private def parseScalar():Boolean = {
    if(currentOrError(BeginScalar)){
      push()
      val textBuilder = new StringBuilder
      var scalarMark = ""
      while(notCurrent(EndScalar)){
        currentToken() match {
          case BeginEscape => textBuilder.append( parseEscaped())
          case Indicator => scalarMark = currentText()
          case Text => textBuilder.append(lexer.tokenText)
          case _ =>
        }
        consume()
      }
      consumeOrError(EndScalar)
      val tagType = if(scalarMark == DoubleQuoteMark.encodeChar.toString) YType.Str.tag else null
      val b     = new YScalar.Builder(textBuilder.toString(),tagType , scalarMark, current.buildParts(), lexer.sourceName) // always enter with begin scalar
      stackParts(buildNode(b.scalar, b.tag))
      true
    }else false
  }

  private def parseList(leftToken: YamlToken, rightToken: YamlToken,parser:ElementParser) = {
    if (consumeOrError(leftToken)) {
      while (notCurrent(rightToken)) {
        parser.parse()
        if (notCurrent(rightToken)) {
          if(currentByTextOrError(Indicator, ","))consume()
        }
      }
      consumeOrError(rightToken)
      true
    }else
      false
  }

  trait ElementParser{
    def parse(): Unit
  }

  case class SequenceValueParser() extends ElementParser{
    override def parse(): Unit = {
      val r = process()
      if(!r) {
        discardIf(Error)
        advanceToByText((Indicator,Some(",")),(EndSequence,None))
      }
    }
  }

  case class MapEntryParser() extends ElementParser {

    override def parse(): Unit = {
      push() // i need new token for YMapEntry container
      if (parseEntry()) stackParts(YMapEntry(current.buildParts()))
      else {
        current.buildParts()
        pop()
      }
    }

    private def parseKey() = {
      val r = parseScalar()
      if(!r) {
        discardIf(Error)
        advanceTo(Indicator, EndMapping)
      }
      r
    }

    private def parseEntry():Boolean = {
      val k = parseKey()
      if (k || currentByText(Indicator, ":")){
        if(currentByTextOrError(Indicator, ":"))consume()
        k & parseValue()
      } else{
        advanceToByText((Indicator,Some(",")), (EndMapping, None))
        false
      }
    }

    private def parseValue(): Boolean  = {
      val r = process()
      if(!r){
        discardIf(Error)
        advanceTo(Indicator, EndMapping)
      }
      r
    }
  }

  private def currentToken(): YamlToken = {
    while (lexer.token == WhiteSpace || lexer.token == LineBreak) {
      current.append()
      lexer.advance()
    }
    lexer.token
  }

  private def currentText(): String = lexer.tokenString

  private def currentRange(): InputRange = lexer.tokenData.range

  private def consume(): Unit = {
    current.append()
    lexer.advance()
  }

  private def discardIf(token: YamlToken): Unit = if(isCurrent(token)) discard()

  private def discard(): Unit = lexer.advance()

  private def advanceIf(token: YamlToken): Unit = if (isCurrent(token)) consume()

  private def advanceTo(tokens:YamlToken*): Unit = {
    while(!eof && !currentAnyOf(tokens: _*)){
      consume()
    }
  }

  private def advanceToByText(tokensText:(YamlToken, Option[String])*): Unit = {
    def current(t:(YamlToken, Option[String])):Boolean = t._2 match {
      case Some(text) => currentByTextOrError(t._1, text)
      case _ => isCurrent(t._1)
    }

    while(!eof && !tokensText.exists(current)){
      consume()
    }
  }

  private def eof() = currentToken() == EndDocument

  private def notCurrent(token: YamlToken) = currentToken() != token && !eof()

  private def currentAnyOf(tokens:YamlToken*) = tokens.contains(currentToken())

  private def isCurrent(token:YamlToken): Boolean = currentToken() == token

  private def currentByText(token: YamlToken, text: String) = isCurrent(token) && currentText() == text

  private def currentByTextOrError(token: YamlToken, text:String): Boolean = {
    if (currentByText(token, text)) true
    else{
       unexpected(text)
      false
    }
  }

  private def currentOrError(token:YamlToken) = {
    if (isCurrent(token)) true
    else{
      unexpected(token)
      false
    }
  }

  private def consumeOrError(token: YamlToken): Boolean = {
    if (currentOrError(token)) {
      consume()
      true
    } else false
  }


  private def buildNode(value: YValue, tag: YTag) = YNode(value, tag, sourceName = lexer.sourceName)


  override protected def newBuilder: JsonBuilder = new JsonBuilder

  class JsonBuilder extends Builder{
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
        val content = YNonContent( rangeFromTo(first.range ,tokens.last.range) , buildTokens(), lexer.sourceName)
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

    private def rangeFromTo(begin:InputRange, end:InputRange) =
      InputRange(begin.lineFrom, begin.columnFrom, end.lineTo, end.columnTo)


  }
}

object JsonParser {
  def apply(s: CharSequence)(implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s))(eh)

  def obj(s: CharSequence)(implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): YObj =
    apply(s)(eh).documents()(0).obj

  def withSource(s: CharSequence, sourceName: String)(implicit eh: ParseErrorHandler =
                                                        ParseErrorHandler.parseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s, sourceName))(eh)

  def withSourceOffset(s: CharSequence, sourceName: String, offset: (Int, Int))(
      implicit eh: ParseErrorHandler = ParseErrorHandler.parseErrorHandler): JsonParser =
    new JsonParser(JsonLexer(s, sourceName, offset))(eh)
}

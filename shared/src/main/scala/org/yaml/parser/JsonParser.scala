package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.TokenData
import org.yaml.lexer.JsonLexer
import org.yaml.lexer.YamlToken.{BeginDocument, _}
import org.yaml.model.{YTag, _}

/**
  * A Json Parser
  */
class JsonParser private[parser] (override val lexer: JsonLexer)(override implicit val eh: ParseErrorHandler) extends BaseParser(lexer) {


  /** Parse the Json and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = { // i can only have one doc in json
    this.keepTokens = keepTokens
    val current = new Builder
    lexer.token match {
      case BeginDocument =>
        while(lexer.token!=EndDocument){
          append(current)
          lexer.advance()
          process().foreach(current.parts += _)
        }
      case _ =>
        val textBuilder = new StringBuilder
        val first = lexer.tokenData
        while (lexer.token != EndDocument) {
          textBuilder.append(lexer.tokenString)
          if(lexer.token!=Error)append(current)
          lexer.advance()
        }
        current.append(TokenData(Error, first rangeTo lexer.tokenData),textBuilder.toString())
        buildParts(current)
    }
    IndexedSeq(YDocument(buildParts(current), lexer.sourceName))
  }

  private def process(): Option[YNode] = {
    val node: Option[YNode] = lexer.token match {
      case BeginSequence => Some(processSeq())
      case BeginMapping => Some(processMap())
      case BeginScalar => Some(processScalar())
      case _          => None
    }
    node
  }

  private def processSeq():YNode = {
    val builder = new Builder
    append(builder)
    lexer.advance()
    while(lexer.token != EndSequence && lexer.token!=EndDocument) {
      lexer.token match {
        case BeginScalar | BeginMapping | BeginSequence => process().foreach(builder.parts += _)
        case WhiteSpace | LineBreak =>
          append(builder)
          lexer.advance()
        case Error =>
          append(builder)
          lexer.advance()
        case Indicator if lexer.tokenString == "," =>
          append(builder)
          lexer.advance()
        case _ =>
          val textBuilder = new StringBuilder
          val first = lexer.tokenData
          val errorBuilder  = new Builder
          def continue = !(Set(EndSequence, EndDocument).contains(lexer.token) || (lexer.token == Indicator && lexer.tokenString == ","))
          while(continue ){
            if(lexer.token != Error) append(errorBuilder)
            textBuilder.append(lexer.tokenString)
            lexer.advance()
          }
          builder.parts ++= errorBuilder.buildParts(TokenData(Error,first rangeTo lexer.tokenData ), textBuilder.toString())
      }
    }
    val v = if(lexer.token == EndSequence) {
      val s = YSequence(buildParts(builder), lexer.sourceName)
      lexer.advance()
      s
    }else
      YSequence(builder.buildParts(TokenData(EndSequence,lexer.tokenData.range)), lexer.sourceName)
    buildNode(v, YType.Seq.tag)
  }

  private def processMap() = {
    val builder = new Builder
    append(builder)
    lexer.advance()
    while(lexer.token != EndMapping && lexer.token !=EndDocument){
      lexer.token match {
        case BeginScalar =>
          processMapEntry().foreach(builder.parts += _)
          skipIgnorables(builder)
          if(lexer.token == Indicator){
            append(builder)
            lexer.advance()
          }
        case WhiteSpace | LineBreak =>
          append(builder)
          lexer.advance()
        case Error =>
          recoverFromMapKey().foreach( builder.parts += _)
        case _ =>
          val textBuilder = new StringBuilder
          val errorBuilder  = new Builder
          while(!Set(EndMapping, EndDocument,BeginScalar).contains(lexer.token) ){
            if(lexer.token != Error) append(errorBuilder)
            textBuilder.append(lexer.tokenString)
            lexer.advance()
          }
          builder.parts ++= errorBuilder.buildParts(TokenData(Error,lexer.tokenData.range ), textBuilder.toString())
      }
    }
    val v = if(lexer. token == EndMapping) {
      val m = YMap(buildParts(builder), lexer.sourceName)
      lexer.advance()
      m
    }else{
      builder.appendCustom(TokenData(Error, lexer.tokenData.range), "Missing closing map")
      YMap(builder.buildParts(TokenData(EndMapping,lexer.tokenData.range), "}"), lexer.sourceName)
    }
    buildNode(v, YType.Map.tag)
  }

  private def skipIgnorables(builder:Builder): Unit = {
    while(lexer.token == WhiteSpace || lexer.token == LineBreak){
      append(builder)
      lexer.advance()
    }
  }
  private def recoverFromMapKey(): Option[YMapEntry] = {
    val entryBuilder = new Builder
    append(entryBuilder)
    lexer.advance()
    while(lexer.token!=Indicator && lexer.token!= EndMapping && lexer.token!=EndDocument){
      append(entryBuilder)
      lexer.advance()
    }
    lexer.token match {
      case Indicator if lexer.tokenString == "," =>
        lexer.advance()
        buildParts(entryBuilder)
        None // not value or key can be parsed, ignore the entire entry
      case Indicator if lexer.tokenString == ":" =>
        entryBuilder.parts += YNode("")
        append(entryBuilder)
        lexer.advance()
        skipIgnorables(entryBuilder)
        entryBuilder.parts += processEntryMapValue()
        val me = YMapEntry(buildParts(entryBuilder))
        if(lexer.token == Indicator)lexer.advance()
        Some(me)
      case _ => None
    }
  }

  private def endMapOrDoc = Set(EndMapping, EndDocument).contains(lexer.token)
  private def keyValueSeparator = lexer.token == Indicator && lexer.tokenString == ":"
  private def entrySeparator = lexer.token == Indicator && lexer.tokenString == ","

  private def processMapEntry():Option[YMapEntry] = {
    val entryBuilder = new Builder
    entryBuilder.parts += processScalar()
    skipIgnorables(entryBuilder)

    if(!keyValueSeparator){
      val textBuilder = new StringBuilder
      val first = lexer.tokenData
      val errorBuilder  = new Builder
      while((!endMapOrDoc) && !keyValueSeparator && !entrySeparator){
        if(lexer.token != Error){
          append(entryBuilder)
        }
        textBuilder.append(lexer.tokenString)
        lexer.advance()
      }
      if(keyValueSeparator)
        entryBuilder.parts ++= errorBuilder.buildParts(TokenData(Error,lexer.tokenData.range), textBuilder.toString())
      else
        entryBuilder.parts ++= errorBuilder.buildParts(TokenData(Error,lexer.tokenData.range ),
          s"Expected ':' found '${textBuilder.toString()+lexer.tokenString}'", custom = true)
    }

    if(keyValueSeparator){
      lexer.advance()
      skipIgnorables(entryBuilder)

      val value = processEntryMapValue()
      entryBuilder.parts += value
      Some(YMapEntry(buildParts(entryBuilder)))
    }else None
  }

  private def processEntryMapValue():YNode = {
    process() match {
      case Some(node) => node
      case _ =>
        val textBuilder = new StringBuilder
        val builder = new Builder
        val first = lexer.tokenData
        if(lexer.token == EndMapping || entrySeparator){
          builder.appendCustom(TokenData(Error,first rangeTo lexer.tokenData), s"Expected value found '${lexer.tokenString}'")
        }else{
          while(!Set(Indicator,EndMapping, EndDocument, BeginScalar).contains(lexer.token) ){
            if(lexer.token !=Error) append(builder)
            textBuilder.append(lexer.tokenString)
            lexer.advance()
          }
          builder.append(TokenData(Error,first rangeTo lexer.tokenData), textBuilder.toString())
        }
        YNode(null,YType.Null.tag,None, buildParts(builder), lexer.sourceName)
    }
  }

  private def processScalar() = {
    val scalarBuilder = new Builder

    var scalarMark = ""
    val textBuilder = new StringBuilder
    while(lexer.token != EndScalar){
      lexer.token match {
        case BeginEscape =>
          val metaTextBuilder = new StringBuilder
          while(lexer.token!= EndEscape){
            lexer.token match {
              case Indicator => metaTextBuilder.append(lexer.tokenString)
              case LineBreak =>
                metaTextBuilder.clear()
              case MetaText =>
                metaTextBuilder.append(lexer.tokenString)
              case _ =>
            }
            append(scalarBuilder)
            lexer.advance()
          }
          //end escape
          textBuilder.append(metaTextBuilder.mkString.decode(ignoreErrors = true))
        case Indicator =>
          scalarMark = lexer.tokenString
        case Text =>
          textBuilder.append(lexer.tokenText)
        case _ =>
      }
      append(scalarBuilder)
      lexer.advance()
    }
    val parts = buildParts(scalarBuilder)
    val b     = new YScalar.Builder(textBuilder.toString(), null, scalarMark, parts, lexer.sourceName) // always enter with begin scalar
    lexer.advance() //advance end scalar
    buildNode(b.scalar,b.tag)
  }

  private def buildNode(value: YValue, tag: YTag) = YNode(value, tag, sourceName = lexer.sourceName)

  private def buildParts(builder: Builder): Array[YPart] = builder.buildParts(lexer.tokenData, lexer.tokenString)

  private def append(builder: Builder) = builder.append(lexer.tokenData, lexer.tokenString)
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

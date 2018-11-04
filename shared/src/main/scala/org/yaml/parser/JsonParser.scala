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
          current.append(lexer.tokenData)
          lexer.advance()
          process().foreach(current.parts += _)
        }
      case _ =>
        val textBuilder = new StringBuilder
        val first = lexer.tokenData
        while (lexer.token != EndDocument) {
          textBuilder.append(lexer.tokenText)
          if(lexer.token!=Error)current.append(lexer.tokenData)
          lexer.advance()
        }
        current.append(TokenData(Error, first rangeTo lexer.tokenData),textBuilder.toString())
        current.buildParts(lexer.tokenData)
    }
    IndexedSeq(YDocument(current.buildParts(lexer.tokenData), lexer.sourceName))
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
    builder.append(lexer.tokenData)
    lexer.advance()
    while(lexer.token != EndSequence && lexer.token!=EndDocument) {
      lexer.token match {
        case BeginScalar | BeginMapping | BeginSequence => process().foreach(builder.parts += _)
        case WhiteSpace | LineBreak =>
          builder.append(lexer.tokenData, lexer.tokenString)
          lexer.advance()
        case Error =>
          //          builder.parts += YNonContent(lexer.tokenData.range, IndexedSeq(AstToken(lexer.token, lexer.tokenText.toString)), lexer.sourceName)
          builder.append(lexer.tokenData, lexer.tokenString)
          lexer.advance()
        case Indicator if lexer.tokenString == "," =>
          builder.append(lexer.tokenData)
          lexer.advance()
        case _ =>
          val textBuilder = new StringBuilder
          val first = lexer.tokenData
          val errorBuilder  = new Builder
          def continue  = {
            !(Set(EndSequence, EndDocument).contains(lexer.token) || (lexer.token == Indicator && lexer.tokenString == ","))
          }
          while(continue ){
            if(lexer.token != Error) errorBuilder.append(lexer.tokenData)
            textBuilder.append(lexer.tokenText)
            lexer.advance()
          }
          builder.parts ++= errorBuilder.buildParts(TokenData(Error,lexer.tokenData.range ), textBuilder.toString())
      }
    }
    val v = if(lexer.token == EndSequence) {
      val s = YSequence(builder.buildParts(lexer.tokenData), lexer.sourceName)
      lexer.advance()
      s
    }else
      YSequence(builder.buildParts(TokenData(EndSequence,lexer.tokenData.range)), lexer.sourceName)
    buildNode(v, YType.Seq.tag)
  }

  private def processMap() = {
    val builder = new Builder
    builder.append(lexer.tokenData)
    lexer.advance()
    while(lexer.token != EndMapping && lexer.token !=EndDocument){
      lexer.token match {
        case BeginScalar =>
          builder.parts += processMapEntry()
        case WhiteSpace | LineBreak =>
          builder.append(lexer.tokenData)
          lexer.advance()
        case Error =>
//          builder.parts += YNonContent(lexer.tokenData.range, IndexedSeq(AstToken(lexer.token, lexer.tokenText.toString)), lexer.sourceName)
          recoverFromMapKey().foreach( builder.parts += _)
        case _ =>
          val textBuilder = new StringBuilder
          val errorBuilder  = new Builder
          while(!Set(EndMapping, EndDocument,BeginScalar).contains(lexer.token) ){
            if(lexer.token != Error) errorBuilder.append(lexer.tokenData)
            textBuilder.append(lexer.tokenText)
            lexer.advance()
          }
          builder.parts ++= errorBuilder.buildParts(TokenData(Error,lexer.tokenData.range ), textBuilder.toString())
      }
    }
    val v = if(lexer. token == EndMapping) {
      val m = YMap(builder.buildParts(lexer.tokenData), lexer.sourceName)
      lexer.advance()
      m
    }else{
      builder.appendCustom(TokenData(Error, lexer.tokenData.range), "Missing closing map")
      YMap(builder.buildParts(TokenData(EndMapping,lexer.tokenData.range)), lexer.sourceName)
    }
    buildNode(v, YType.Map.tag)
  }

  private def skipIgnorables(builder:Builder): Unit = {
    while(lexer.token == WhiteSpace || lexer.token == LineBreak){
      builder.append(lexer.tokenData)
      lexer.advance()
    }
  }
  private def recoverFromMapKey(): Option[YMapEntry] = {
    val entryBuilder = new Builder
    entryBuilder.append(lexer.tokenData, lexer.tokenString)
    entryBuilder.parts += YNode("")
    lexer.advance()
    while(lexer.token!=Indicator && lexer.token!= EndMapping && lexer.token!=EndDocument){
      entryBuilder.append(lexer.tokenData)
      lexer.advance()
    }
    lexer.token match {
      case Indicator if lexer.tokenString == "," =>
        lexer.advance()
        entryBuilder.buildParts(lexer.tokenData)
        None // not value or key can be parsed, ignore the entire entry
      case Indicator if lexer.tokenString == ":" =>
        entryBuilder.append(lexer.tokenData)
        lexer.advance()
        skipIgnorables(entryBuilder)
        entryBuilder.parts += processEntryMapValue()
        val me = YMapEntry(entryBuilder.buildParts(lexer.tokenData))
        if(lexer.token == Indicator)lexer.advance()
        Some(me)
      case _ => None
    }
  }

  private def processMapEntry() = {
    val entryBuilder = new Builder
    entryBuilder.parts += processScalar()
    skipIgnorables(entryBuilder)
    lexer.token match {
      case Indicator if lexer.tokenString == ":" =>
        entryBuilder.append(lexer.tokenData)
        lexer.advance()
      case _ =>
        val textBuilder = new StringBuilder
        val first = lexer.tokenData
        val errorBuilder  = new Builder
        while(!Set(Indicator,EndMapping, EndDocument).contains(lexer.token) ){
          if(lexer.token != Error){
            errorBuilder.append(lexer.tokenData)
          }
          textBuilder.append(lexer.tokenText)
          lexer.advance()
        }
        entryBuilder.parts ++= errorBuilder.buildParts(TokenData(Error,lexer.tokenData.range ), textBuilder.toString())
    }
    skipIgnorables(entryBuilder)
    val value = processEntryMapValue()
    skipIgnorables(entryBuilder)
    entryBuilder.parts += value
    val me = YMapEntry(entryBuilder.buildParts(lexer.tokenData))
    if(lexer.token == Indicator)lexer.advance()
    me
  }

  private def processEntryMapValue():YNode = {
    process() match {
      case Some(node) => node
      case _ =>
        val textBuilder = new StringBuilder
        val builder = new Builder
        val first = lexer.tokenData
        while(!Set(Indicator,EndMapping, EndDocument, BeginScalar).contains(lexer.token) ){
          builder.append(lexer.tokenData)
          textBuilder.append(lexer.tokenText)
          lexer.advance()
        }
        builder.append(TokenData(Error,first rangeTo lexer.tokenData), lexer.tokenString)
        YNode(null,YType.Null.tag,None,builder.buildParts(lexer.tokenData), lexer.sourceName)
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
              case Indicator => metaTextBuilder.append(lexer.tokenText)
              case LineBreak =>
                metaTextBuilder.clear()
              case MetaText =>
                metaTextBuilder.append(lexer.tokenText)
              case _ =>
            }
            scalarBuilder.append(lexer.tokenData)
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
      scalarBuilder.append(lexer.tokenData)
      lexer.advance()
    }
    val parts = scalarBuilder.buildParts(lexer.tokenData)
    val b     = new YScalar.Builder(textBuilder.toString(), null, scalarMark, parts, lexer.sourceName) // always enter with begin scalar
    lexer.advance() //advance end scalar
    buildNode(b.scalar,b.tag)
  }

  private def buildNode(value: YValue, tag: YTag) = YNode(value, tag, sourceName = lexer.sourceName)

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

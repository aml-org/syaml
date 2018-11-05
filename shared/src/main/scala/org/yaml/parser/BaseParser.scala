package org.yaml.parser
import org.mulesoft.lexer.{AstToken, BaseLexer, TokenData}
import org.yaml.lexer.YamlToken
import org.yaml.lexer.YamlToken._
import org.yaml.model._

import scala.collection.mutable.ArrayBuffer

abstract class BaseParser private[parser] (val lexer: BaseLexer[YamlToken])(implicit val eh: ParseErrorHandler) {
  type TD = TokenData[YamlToken]
  type B
  protected var keepTokens                        = false
  protected var current: B = newBuilder
  protected var stack                             = List(current)

  def parse(keepTokens: Boolean = true): IndexedSeq[YPart]

  /** Parse the Json and return the list of documents */
  def documents(): IndexedSeq[YDocument] = {
    val parts = parse(keepTokens = false)
    // Merge header into first document
    val header = parts.takeWhile(p => !p.isInstanceOf[YDocument])
    val docs: Array[YDocument] =
      parts.collect({ case d: YDocument => d })(collection.breakOut)
    if (docs.nonEmpty) docs(0) = YDocument(header ++ docs(0).children, lexer.sourceName)
    docs
  }

  protected def newBuilder: B
  protected abstract class Builder {

    var first: TD               = _
    val tokens                  = new ArrayBuffer[AstToken]
    val parts                   = new ArrayBuffer[YPart]
    var value: YValue           = _

    def append(td: TD, text: String = ""): Unit = {
      if (keepTokens) tokens += AstToken(td.token, text, td.range)
      if (first == null) first = td
    }

    def appendCustom(td: TD, text: String): Unit = {
      if (keepTokens) tokens += AstToken(td.token, text, td.range, parsingError = true)
      if (first == null) first = td
    }

    def buildTokens(td: TD = null): IndexedSeq[AstToken] = {
      if (td != null) this append td
      if (tokens.isEmpty) IndexedSeq.empty
      else {
        val r = tokens.toArray[AstToken]
        tokens.clear()
        first = null
        r
      }
    }

    def addNonContent(td: TD): Unit =
      if (tokens.nonEmpty) {
        val content = YNonContent(first rangeTo td, buildTokens(), lexer.sourceName)
        parts += content
        collectErrors(content)
      }

    def collectErrors(nonContent: YNonContent): Unit = {
      nonContent.tokens.find(_.tokenType == Error) match {
        case Some(astToken: AstToken) =>
          eh.handle(YNonContent(astToken.range, IndexedSeq(astToken), lexer.sourceName), if(astToken.parsingError)ParserException(astToken.text) else LexerException(astToken.text))
        case _ =>
      }
    }
  }
}

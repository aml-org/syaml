package org.yaml.parser
import org.mulesoft.lexer.{AstToken, BaseLexer, TokenData}
import org.yaml.lexer.YamlToken
import org.yaml.lexer.YamlToken._
import org.yaml.model._

import scala.collection.mutable.ArrayBuffer

abstract class BaseParser private[parser] (val lexer: BaseLexer[YamlToken])(implicit val eh: ParseErrorHandler) {
  type TD = TokenData[YamlToken]

  protected var keepTokens                        = false

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

  protected class Builder {
    var first: TD               = _
    val tokens                  = new ArrayBuffer[AstToken]
    val parts                   = new ArrayBuffer[YPart]
    var anchor: Option[YAnchor] = None
    var alias: String           = ""
    var tag: YTag               = _
    var value: YValue           = _

    def append(td: TD, text: String = ""): Unit = {
      if (keepTokens) tokens += AstToken(td.token, text, td.range)
      if (first == null) first = td
    }

    def appendCustom(td: TD, text: String = ""): Unit = {
      if (keepTokens) tokens += AstToken(td.token, text, td.range, true)
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
    def buildParts(td: TD, text: String = ""): Array[YPart] = {
      this append (td, text)
      addNonContent(td)
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray[YPart]
        parts.clear()
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

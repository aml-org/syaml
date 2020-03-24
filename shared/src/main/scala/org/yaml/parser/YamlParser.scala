package org.yaml.parser

import org.mulesoft.lexer._
import org.yaml.lexer.YamlLexer
import org.yaml.model._

/**
  * A Yaml Parser that covers Steps Parse and Compose of the spec.
  * [[http://www.yaml.org/spec/1.2/spec.html#id2762107 Yaml 1.2 Processes]]
  */
class YamlParser private[parser] (val lexer: YamlLexer)(implicit val eh: ParseErrorHandler) extends YParser {
  private var includeTag = ""

  /** Parse the Yaml and return the list of documents */
  def documents(): IndexedSeq[YDocument] = {
    val parts = parse(keepTokens = false)
    // Merge header into first document
    val header = parts.takeWhile(p => !p.isInstanceOf[YDocument])
    val docs: Array[YDocument] =
      parts.collect({ case d: YDocument => d })(collection.breakOut)
    if (docs.nonEmpty) docs(0) = new YDocument(SourceLocation(lexer.sourceName), header ++ docs(0).children)
    docs
  }

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = parse(keepTokens, StreamLexerContext)

  private def parse(keepTokens:  Boolean, ctx:LexerContext):IndexedSeq[YPart] =
    new YamlLoader(lexer.initialize(ctx), keepTokens, includeTag, eh).parse()

  override def document(): YDocument = {
    new YDocument(SourceLocation(lexer.sourceName), parse(keepTokens = false, SingleDocumentLexerContext))
  }

  /** Define an Include Tag if not empty it will generate Mutable Node References for tagged nodes */
  def withIncludeTag(s: String): this.type = {
    includeTag = s
    this
  }
}

object YamlParser {

  def apply(lexer: YamlLexer)(implicit eh: ParseErrorHandler): YamlParser =
    new YamlParser(lexer)(eh)
  def apply(s: CharSequence)(implicit eh: ParseErrorHandler): YamlParser =
    apply(YamlLexer(s))(eh)
  def apply(s: CharSequence, sourceName: String)(implicit eh: ParseErrorHandler): YamlParser =
    apply(YamlLexer(s, sourceName))(eh)

  def apply(s: CharSequence, sourceName: String, offset: Position)(implicit eh: ParseErrorHandler): YamlParser =
    apply(YamlLexer(s, sourceName, offset))(eh)

  @deprecated("Use Position argument", "")
  def apply(s: CharSequence, sourceName: String, offset: (Int, Int))(implicit eh: ParseErrorHandler): YamlParser =
    YamlParser(s, sourceName, Position(offset._1, offset._2))(eh)

}

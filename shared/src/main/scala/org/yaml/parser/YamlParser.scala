package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{BaseLexer, InputRange, TokenData}
import org.yaml.lexer.YamlToken._
import org.yaml.lexer.{YamlLexer, YamlToken}
import org.yaml.model
import org.yaml.model.{YTag, _}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A Yaml Parser that covers Steps Parse and Compose of the spec.
  * [[http://www.yaml.org/spec/1.2/spec.html#id2762107 Yaml 1.2 Processes]]
  */
class YamlParser private[parser] (override val lexer: BaseLexer[YamlToken])(override implicit val eh: ParseErrorHandler) extends BaseParser(lexer) {

  private val aliases                           = mutable.Map.empty[String, YNode]
  private var escaping                          = false
  private val textBuilder                       = new StringBuilder
  private val metaTextBuilder                   = new StringBuilder
  private var inHandle                          = false
  private var inTag                             = false
  private var scalarMark                        = ""
  private var current                           = new Builder
  private var stack                             = List(current)
  private var prev: TD                          = TokenData(BeginStream, InputRange.Zero)
  private var lastBegin                         = BeginStream
  private var directiveArgs: ListBuffer[String] = _
  private var includeTag                        = ""

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  override def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = {
    this.keepTokens = keepTokens
    stack = List(current)
    while (lexer.token != EndStream) {
      prev = process(lexer.tokenData)
      lexer.advance()
    }
    current.addNonContent(prev)
    current.parts.toArray[YPart]
  }

  /** Define an Include Tag if not empty it will generate Mutable Node References for tagged nodes */
  def withIncludeTag(s: String): this.type = {
    includeTag = s
    this
  }

  private def push(td: TD): Unit = {
    current.addNonContent(prev)
    metaTextBuilder.clear()
    textBuilder.clear()
    lastBegin = td.token
    current = new Builder
    stack = current :: stack
  }

  private def pop(part: YPart): Unit = {
    stack = stack.tail
    current = stack.head
    current.parts += part
  }

  private def buildText(): String = {
    val r = textBuilder.toString()
    textBuilder.clear()
    r
  }
  private def buildMetaText(): String = {
    val r = metaTextBuilder.mkString
    metaTextBuilder.clear()
    r
  }

  private def process(td: TD): TD = {
    td.token match {
      case BeginDocument =>
        aliases.clear()
        push(td)
      case BeginNode | BeginComment | BeginSequence | BeginScalar | BeginMapping | BeginPair | BeginAlias |
          BeginAnchor =>
        push(td)
      case BeginTag =>
        inTag = true
        push(td)
      case BeginDirective =>
        directiveArgs = ListBuffer.empty
        push(td)
      case EndDocument  => pop(YDocument(current.buildParts(td), lexer.sourceName)); return td
      case EndComment   => return createComment(td)
      case EndSequence  => return createSequence(td)
      case EndNode      => return createNode(td)
      case EndScalar    => return createScalar(td)
      case EndMapping   => return createMap(td)
      case EndPair      => return createPair(td)
      case EndAlias     => return createAlias(td)
      case EndAnchor    => return createAnchor(td)
      case EndTag       => return createTag(td)
      case EndDirective => return createDirective(td)
      case Text         => textBuilder.append(lexer.tokenText)
      case LineFold     => textBuilder.append(' ')
      case LineFeed     => textBuilder.append('\n')
      case BeginEscape  => escaping = true
      case BeginHandle  => inHandle = true
      case EndHandle    => inHandle = false
      case MetaText =>
        metaTextBuilder.append(lexer.tokenText)
      case Indicator =>
        if (escaping || inHandle || inTag)
          metaTextBuilder.append(lexer.tokenText)
        else if (prev.token == BeginScalar) scalarMark = lexer.tokenString
      case EndEscape =>
        textBuilder.append(buildMetaText().decode(ignoreErrors = true))
        escaping = false
      case LineBreak =>
        if (escaping) metaTextBuilder.clear()
      case WhiteSpace => addDirectiveArg()
      case _          =>
    }
    current.append(td, lexer.tokenString)
    td
  }

  private def addDirectiveArg(): Unit = {
    if (directiveArgs != null && metaTextBuilder.nonEmpty) {
      directiveArgs += metaTextBuilder.result
      metaTextBuilder.clear()
    }
  }

  private def createComment(td: TD) = {
    pop(YComment(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
    td
  }
  private def createDirective(td: TD) = {
    addDirectiveArg()
    val parts = current.buildParts(td)
    parts collectFirst { case YTag(tag, _, _, _) => directiveArgs += tag }
    pop(YDirective(directiveArgs.head, directiveArgs.tail.toArray[String], parts, lexer.sourceName))
    metaTextBuilder.clear()
    directiveArgs = null
    td
  }

  private def createTag(td: TokenData[YamlToken]) = {
    inTag = false
    val t =
      YTag(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(t)
    current.tag = t
    td
  }

  private def createAnchor(td: TD) = {
    val anchor = YAnchor(buildMetaText(), current.first rangeTo td, current.buildTokens(td), lexer.sourceName)
    pop(anchor)
    current.anchor = Some(anchor)
    td
  }

  private def createPair(td: TD) = {
    pop(YMapEntry(current.buildParts(td)))
    td
  }

  private def createSequence(td: TD) = {
    val v = YSequence(current.buildParts(td), lexer.sourceName)
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Seq)
    td
  }

  private def createNode(td: TD) = {
    val parts = current.buildParts(td)
    if (current.alias.nonEmpty) {
      val target = aliases.getOrElse(current.alias, YNode.Null)
      // Manage Error if (target == YNode.Null)
      pop(new YNode.Alias(current.alias, target, parts))
    }
    else {
      val tag = current.tag
      val n =
        if (includeTag.nonEmpty && tag.text == includeTag)
          new model.YNode.MutRef(current.value, tag, parts)
        else YNode(current.value, tag, current.anchor, parts, lexer.sourceName)
      for (a <- current.anchor) aliases += a.name -> n
      pop(n)
    }
    td
  }

  private def createMap(td: TD) = {

    val v = YMap(current.buildParts(td), lexer.sourceName)
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Map)
    td
  }

  private def createAlias(td: TD) = {
    val aliasName = buildMetaText()
    val alias     = YAnchor(aliasName, current.first rangeTo td, current.buildTokens(td), lexer.sourceName)
    pop(alias)
    current.alias = aliasName
    td
  }

  private def createScalar(td: TD): TD = {
    stack = stack.tail
    val c = stack.head

    val parts = current.buildParts(td)
    val b     = new YScalar.Builder(buildText(), c.tag, scalarMark, parts, lexer.sourceName)
    c.value = b.scalar
    c.tag = b.tag
    c.parts += b.scalar

    current = c

    scalarMark = ""
    td
  }
  private def tagFor(tag: YTag, defaultType: YType) =
    if (tag == null) defaultType.tag
    else if (tag.tagType == YType.Empty) tag.copy(tagType = defaultType)
    else tag

}

object YamlParser {

  def apply(lexer: YamlLexer)(implicit eh: ParseErrorHandler): YamlParser =
    new YamlParser(lexer)(eh)
  def apply(s: CharSequence)(implicit eh: ParseErrorHandler): YamlParser =
    apply(YamlLexer(s))(eh)
  def apply(s: CharSequence, sourceName: String)(implicit eh: ParseErrorHandler): YamlParser =
    apply(YamlLexer(s, sourceName))(eh)
  def apply(s: CharSequence, sourceName: String, offset: (Int, Int))(implicit eh: ParseErrorHandler): YamlParser =
    apply(YamlLexer(s, sourceName, offset))(eh)
}
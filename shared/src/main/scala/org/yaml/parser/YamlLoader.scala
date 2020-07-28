package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{AstToken, SourceLocation}
import org.yaml.lexer.YamlLexer
import org.yaml.lexer.YamlToken._
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

private[parser] class YamlLoader(val lexer: YamlLexer,
                                 val keepTokens: Boolean,
                                 val includeTag: String,
                                 implicit val eh: ParseErrorHandler) {

  private val aliases              = mutable.Map.empty[String, YNode]
  private def current: YamlBuilder = stack.top()
  private val stack                = new Stack

  def parse(): IndexedSeq[YPart] = {
    stack.push(new YamlBuilder())
    while (lexer.token != EndStream) {
      process()
      lexer.advance()
    }
    stack.pop().buildParts()
  }

  private def push(e: YamlBuilder = new YamlBuilder()): Unit = {
    current.addNonContent()
    stack.push(e)
  }

  private def pop(part: YPart): Unit = {
    stack.pop()
    current.parts += part
  }

  private def process(): Unit = {
    lexer.token match {

      case BeginDocument  => push(new DocBuilder)
      case BeginNode      => push(new NodeBuilder)
      case BeginScalar    => push(new ScalarBuilder(current.tag))
      case BeginSequence  => push(new SeqBuilder)
      case BeginPair      => push(new PairBuilder)
      case BeginMapping   => push(new MapBuilder)
      case BeginDirective => push(new DirectiveBuilder)
      case BeginComment   => push(new CommentBuilder)
      case BeginAlias     => push(new AliasBuilder)
      case BeginAnchor    => push(new AnchorBuilder())
      case BeginTag       => push(new TagBuilder)

      case EndScalar | EndDirective | EndMapping | EndSequence | EndDocument | EndNode | EndPair | EndComment |
          EndAlias | EndAnchor | EndTag =>
        current.create()
        return

      case Text        => current.appendText(lexer.tokenText)
      case LineFold    => current.appendText(" ")
      case LineFeed    => current.appendText("\n")
      case BeginEscape => current.escaping = true
      case BeginHandle => current.inHandle = true
      case EndHandle   => current.inHandle = false
      case MetaText    => current.appendMetaText()
      case Indicator   => current.processIndicator()
      case EndEscape   => current.endEscape()
      case LineBreak   => current.processLineBreak()
      case WhiteSpace  => current.processWhiteSpace()
      case Error       => eh.handle(lexer.tokenData.range, LexerException(lexer.tokenString))
      case _           =>
    }
    current.addCurrentToken()
  }

  private def duplicates(parts: Array[YPart]): Unit = {
    val keys = mutable.Set[String]()
    for (part <- parts) part match {
      case entry: YMapEntry =>
        entry.key.value match {
          case s: YScalar =>
            val key = s.text
            if (!keys.add(key)) eh.handle(entry.key.location, DuplicateKeyException(key))
          case _ =>
        }
      case _ =>
    }
  }

  private class YamlBuilder(val first: SourceLocation = lexer.tokenData.range) {
    val tokens          = new ArrayBuffer[AstToken]
    val parts           = new ArrayBuffer[YPart]
    val metaTextBuilder = new StringBuilder
    var escaping        = false
    var inHandle        = false

    var value: YValue = _

    var anchor: Option[YAnchor] = None
    var alias: String           = ""
    var tag: YTag               = _

    def processWhiteSpace() {}
    def appendText(txt: CharSequence): Unit = {}
    def appendMetaText(): Unit              = metaTextBuilder.append(lexer.tokenText)
    def processLineBreak(): Unit            = if (escaping) current.metaTextBuilder.clear()
    def create(): Unit                      = {}

    def setValue(v: YValue, defaultType: YType): Unit = {
      value = v
      if (tag == null) tag = defaultType.tag
      else if (tag.tagType == YType.Empty) tag = tag.withTag(tagType = defaultType)
    }

    def setValue(v: YValue, t: YTag): Unit = {
      value = v
      tag = t
    }

    def endEscape(): Unit = {
      appendText(metaTextBuilder.result().decode(ignoreErrors = true))
      metaTextBuilder.clear()
      escaping = false
    }

    def processIndicator(): Unit = if (escaping || inHandle) metaTextBuilder.append(lexer.tokenText)

    def buildParts(): Array[YPart] = {
      addCurrentToken()
      addNonContent()
      createArray(parts)
    }
    private def createArray[P: ClassTag](parts: ArrayBuffer[P]): Array[P] =
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray
        parts.clear()
        r
      }

    def addCurrentToken(): Unit = {
      val td = lexer.tokenData
      if (keepTokens && td.token != EndStream) tokens += AstToken(td.token, lexer.tokenString, td.range)
    }

    def buildTokens(): IndexedSeq[AstToken] = createArray(tokens)

    def addNonContent(): Unit =
      if (tokens.nonEmpty) parts += YNonContent(createArray(tokens))

    def location(): SourceLocation = first to lexer.tokenData.range
  }

  private class DocBuilder extends YamlBuilder {
    aliases.clear()
    override def create(): Unit = pop(new YDocument(location(), buildParts()))
  }

  private class NodeBuilder extends YamlBuilder {
    override def create(): Unit = {
      val parts = buildParts()
      val loc   = location()
      val node =
        if (alias.nonEmpty) new YNode.Alias(alias, aliases.getOrElse(alias, {
          eh.handle(loc, UndefinedAnchorException(alias))
          YNode.Null
        }), loc, parts)
        else if (includeTag.nonEmpty && tag.text == includeTag) new YNode.MutRef(value, tag, loc, parts)
        else new YNodePlain(value, tag, anchor, loc, parts)
      for (a <- anchor) aliases += a.name -> node
      pop(node)
    }
  }

  private class ScalarBuilder(t: YTag) extends YamlBuilder {
    tag = t
    private val text             = new StringBuilder()
    private var mark: ScalarMark = UnknownMark

    override def processIndicator(): Unit = {
      if (mark == UnknownMark)
        mark = ScalarMark(lexer.tokenString)
      super.processIndicator()
    }

    override def appendText(txt: CharSequence): Unit = {
      if (mark == UnknownMark) mark = NoMark
      text.append(txt)
    }

    override def create(): Unit = {
      val txt = text.result()
      val m   = if (mark == UnknownMark) NoMark else mark
      val loc = location()

      val r = ScalarParser.parse(txt, m, tag, loc)
      val scalar = new YScalar(r.value, txt, m, loc, buildParts())
      pop(scalar)
      current.setValue(scalar, r.tag)

    }
  }

  private class DirectiveBuilder extends YamlBuilder {
    private val args = ListBuffer.empty[String]

    override def processWhiteSpace(): Unit = addArg()

    private def addArg(): Unit = if (metaTextBuilder.nonEmpty) {
      args += metaTextBuilder.result()
      metaTextBuilder.clear()
    }

    override def create(): Unit = {
      addArg()
      val parts = buildParts()
      parts collectFirst { case t: YTag => args += t.text }
      pop(YDirective(args.head, args.tail.toArray[String], location(), parts))
    }
  }

  private class SeqBuilder extends YamlBuilder {
    override def create(): Unit = {
      val v = YSequence(location(), buildParts())
      pop(v)
      current.setValue(v, YType.Seq)
    }
  }

  private class MapBuilder extends YamlBuilder {
    override def create(): Unit = {
      val parts = buildParts()
      duplicates(parts)
      val v = YMap(location(), parts)
      pop(v)
      current.setValue(v, YType.Map)
    }
  }

  private class PairBuilder extends YamlBuilder {
    override def create(): Unit = pop(YMapEntry(location(), buildParts()))
  }

  private class CommentBuilder extends YamlBuilder {
    override def create(): Unit = {
      addCurrentToken()
      pop(YComment(metaTextBuilder.result(), location(), buildTokens()))
    }
  }
  private class AliasBuilder extends YamlBuilder {
    override def create(): Unit = {
      val aliasName = metaTextBuilder.result()
      addCurrentToken()
      pop(YAnchor(aliasName, location(), buildTokens()))
      current.alias = aliasName
    }
  }
  private class AnchorBuilder extends YamlBuilder {
    override def create(): Unit = {
      addCurrentToken()
      val anchor =
        YAnchor(metaTextBuilder.result(), location(), buildTokens())
      pop(anchor)
      current.anchor = Some(anchor)
    }
  }

  private class TagBuilder extends YamlBuilder {
    override def processIndicator(): Unit = metaTextBuilder.append(lexer.tokenText)
    override def create(): Unit = {
      addCurrentToken()
      val t =
        YTag(metaTextBuilder.result(), location(), current.buildTokens())
      pop(t)
      current match {
        case n: NodeBuilder => n.tag = t
        case _              =>
      }
    }
  }

  private class Stack {
    private var list = List(new YamlBuilder)

    def push(e: YamlBuilder): Unit = {
      list = e :: list
    }

    def pop(): YamlBuilder = {
      val r = list.head
      list = list.tail
      r
    }
    def top(): YamlBuilder = list.head
  }

}

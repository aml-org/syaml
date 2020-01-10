package org.yaml.lexer

import java.lang.Integer.MAX_VALUE

import org.mulesoft.common.core.countWhile
import org.mulesoft.lexer.LexerInput.{EofChar, Mark}
import org.mulesoft.lexer._
import org.yaml.lexer.YamlCharRules._
import org.yaml.lexer.YamlToken._

import scala.annotation.tailrec

/**
  * Yaml Lexer for 1.2 Specification
  */
final class YamlLexer private (input: LexerInput, positionOffset: Position = Position.Zero)
    extends BaseLexer[YamlToken](input, positionOffset) {

  //~ Methods ..........................................................................................................

  override protected def initialize(): Unit = {
    yamlStream()
    advance()
  }

  override protected def processPending(): Unit = emit(EndStream)

  def emitIndicator(): Boolean = consumeAndEmit(Indicator)

  /** Check that the current char is the specified one and if so emit it as an Indicator */
  @failfast def indicator(chr: Int): Boolean = currentChar == chr && consumeAndEmit(Indicator)


  //  def ensureDocumentStarted(): Boolean =
  //    if (stack.exists(s => s.isInstanceOf[InDocument])) false
  //    else {
  //      InDocument()(this)
  //      true
  //    }

  /**
    * Recognition of lineBreak Sequence<p>
    *
    * [28] b-break	::=	( b-carriage-return b-line-feed ) | b-line-feed
    */
  def lineBreakSequence(offset: Int = 0): Int = {
    val current = lookAhead(offset)
    if (current == '\n') 1 else if (current == '\r' && lookAhead(offset + 1) == '\n') 2 else 0
  }

  /** Utility method to Process the lineBreak */
  @failfast private def lineBreak(t: YamlToken): Boolean = {
    val n = lineBreakSequence()
    n != 0 && {
      consume(n); emit(t)
    }
  }

  /**
    * Line Break normalized to lineFeed<p>
    *
    * [29] b-as-line-feed	::=	b-break
    */
  def breakAsLineFeed(): Boolean = lineBreak(LineFeed)

  /**
    * Outside scalar content, YAML allows any line break to be used to terminate lines.<p>
    * [30]	b-non-content	::=	b-break
    */
  @failfast def breakNonContent(): Boolean = lineBreak(LineBreak)

  /**
    * URI characters for tags, as specified in RFC2396,
    * with the addition of the “[” and “]” for presenting IPv6 addresses as proposed in RFC2732.<p>
    * [39]	ns-uri-char	::=	  “%” ns-hex-digit ns-hex-digit | ns-word-char
    * | “#” | “;” | “/” | “?” | “:” | “@” | “&” | “=” | “+” | “$”
    * | “,” | “_” | “.” | “!” | “~” | “*” | “'” | “(” | “)” | “[” | “]”
    */
  @failfast private def uriChar() =
    if (currentChar >= 0  && ("#;/?:@&=+$,_.!~*'()[]".indexOf(currentChar) != -1 || isWordChar(currentChar))) {
      consume()
      true
    }
    else if (currentChar == '%' && isNsHexDigit(lookAhead(1)) && isNsHexDigit(lookAhead(2))) {
      consume(3)
      true
    }
    else false

  /**
    * A Tag char cannot contain the “!” character because is used to indicate the end of a named tag handle.
    * In addition, the “[”, “]”, “{”, “}” and “,” characters are excluded
    * because they would cause ambiguity with flow collection structures.<p>
    *
    * [40] ns-tag-char ::= [[uriChar ns-uri-char]] - “!” - [[isFlowIndicator c-flow-indicator]]
    */
  @failfast private def tagChar() = currentChar != '|' && !isFlowIndicator(currentChar) && uriChar()

  /**
    * Process an indentation exactly as the current one<p>
    * [63]	s-indent(n)	::=	s-space × n
    */
  def indent(n: Int): Boolean = n <= 0 || n == input.countSpaces(0, n) && consumeAndEmit(n, Indent)

  /**
    * Detect an indentation lower than the current one<p>
    *
    * [64]	s-indent(&lt;n)	::=	s-space × m (Where m < n)
    */
  def indentLess(n: Int): Boolean = {
    val m = input.countSpaces(0, MAX_VALUE)
    m < n && consumeAndEmit(m, Indent)
  }

  /**
    * Detect an indentation lower or equal to the current one<p>
    *
    * [64]	s-indent(&le;n)	::=	s-space × m (Where m ≤ n)
    */
  def indentLessOrEqual(n: Int): Boolean = {
    val m = input.countSpaces(0, MAX_VALUE)
    m <= n && consumeAndEmit(m, Indent)
  }

  /**
    * [66]	s-separate-in-line	::=	s-white+ | /* Start of line */
    */
  @failfast private def separateInLine(): Boolean = {
    val n = input.countWhiteSpaces()
    n > 0 && consumeAndEmit(n, WhiteSpace) || beginOfLine
  }

  /**
    * <blockquote><pre>
    * [67]	s-line-prefix(n,c)	::=
    * c = block-out ⇒ s-block-line-prefix(n)
    * c = block-in  ⇒ s-block-line-prefix(n)
    * c = flow-out  ⇒ s-flow-line-prefix(n)
    * c = flow-in   ⇒ s-flow-line-prefix(n)
    * [68]	s-block-line-prefix(n)	::=	s-indent(n)
    * [69]	s-flow-line-prefix(n)	::=	s-indent(n) s-separate-in-line?
    * [70]	l-empty(n,c)	::=	( s-line-prefix(n,c) | s-indent(&lt;n) ) b-as-line-feed
    *
    * </blockquote></pre>
    */
  def emptyLine(n: Int, ctx: YamlContext): Boolean =
    linePrefix(n, ctx == FlowOut || ctx == FlowIn, emptyLine = true) && breakAsLineFeed()

  private def linePrefix(n: Int, flow: Boolean, emptyLine: Boolean = false): Boolean =
    if (!beginOfLine) false
    else {
      if (isDirectivesEnd || isDocumentEnd) return false
      val spaces = input.countSpaces(0, n)
      if (!emptyLine && spaces < n) return false
      val whiteSpaces = if (flow) input.countWhiteSpaces(spaces) else 0
      if (emptyLine && !isBBreak(lookAhead(spaces + whiteSpaces))) false
      else {
        consumeAndEmit(spaces, Indent)
        consumeAndEmit(whiteSpaces, WhiteSpace)
      }
    }

  private def flowLinePrefix(): Boolean = {

    if (isDirectivesEnd || isDocumentEnd) return false
    val spaces      = input.countSpaces(0, MAX_VALUE)
    val whiteSpaces = input.countWhiteSpaces(spaces)
    consumeAndEmit(spaces, Indent)
    consumeAndEmit(whiteSpaces, WhiteSpace)
  }

  /**
    * If a line break is followed by an empty line, it is trimmed.
    * The first line break is discarded and the rest are retained as content.<p>
    * [71]	b-l-trimmed(n,c)	::=	[[breakNonContent b-non-content]] [[emptyLine l-empty(n,c)]]+
    */
  private def trimmed(n: Int, ctx: YamlContext): Boolean = matches(breakNonContent() && oneOrMore(emptyLine(n, ctx)))

  /**
    * Convert a lineBreak to an space (fold it)<p>
    * [72]   	b-as-space	::=	b-break
    */
  def breakAsSpace(): Boolean = lineBreak(LineFold)

  /**
    * A folded non-empty line may end with either of the above line breaks.<p>
    * [73]	b-l-folded(n,c)	::=	[[trimmed b-l-trimmed(n,c)]] | [[breakAsSpace b-as-space]]
    */
  private def folded(n: Int, ctx: YamlContext): Boolean =
    isBBreak(currentChar) && (matches(trimmed(n, ctx)) || breakAsSpace())

  /**
    * The combined effect of the flow line folding rules is that each “paragraph” is interpreted as a line,
    * empty lines are interpreted as line feeds,
    * and text can be freely more-indented without affecting the content information<p>
    *
    * [74]	s-flow-folded(n)	::=	[[separateInLine s-separate-in-line]]?
    * [[folded b-l-folded(n,flow-in)]]
    * [[linePrefix s-flow-line-prefix(n)]]
    */
  private def flowFolded(n: Int): Boolean = {
    separateInLine()
    folded(n, FlowIn) && linePrefix(n, flow = true)
  }

  /**
    * [75]	c-nb-comment-text	::=	“#” nb-char*
    *
    * Actually it is doing:
    * commentText ::= '#' nb-char* b-comment
    */
  @failfast def commentText(): Boolean = {
    if (currentChar != '#') return false
    emit(BeginComment)
    consumeAndEmit(Indicator)
    while (!isBreakComment(currentChar)) consume()
    emit(MetaText)
    emit(EndComment)
    breakComment()
  }

  /**
    * [76]	b-comment	::=	[[breakNonContent b-non-content]] | EofChar
    */
  @failfast def breakComment(): Boolean = currentChar == EofChar || breakNonContent()

  /**
    * [77]	s-b-comment	::=	( s-separate-in-line c-nb-comment-text? )? b-comment*
    * Actually refactored to:
    * b-comment | l-comment
    */
  @failfast def spaceBreakComment(): Boolean = breakComment() || lineComment()

  /**
    * <blockquote><pre>
    *
    * [78]	l-comment	::=	s-separate-in-line c-nb-comment-text? b-comment
    *
    * Actually refactored to:
    * [[separateInLine s-separate-in-line]] ([[breakComment b-non-content]] | [[commentText]])
    * </blockquote></pre>
    *
    */
  @failfast def lineComment(): Boolean = separateInLine() && (breakComment() || commentText())

  /**
    * [79]	s-l-comments	::=	( s-b-comment | /* Start of line */ ) l-comment*
    */
  @failfast def multilineComment(): Boolean = (beginOfLine || spaceBreakComment()) && zeroOrMore(lineComment())

  /**
    * [80]	s-separate(n,c)	::=	c = block-out ⇒ s-separate-lines(n)
    * c = block-in  ⇒ s-separate-lines(n)
    * c = flow-out  ⇒ s-separate-lines(n)
    * c = flow-in   ⇒ s-separate-lines(n)
    * c = block-key ⇒ s-separate-in-line
    * c = flow-key  ⇒ s-separate-in-line
    * [81]	s-separate-lines(n)	::=	  ( s-l-comments s-flow-line-prefix(n) ) | s-separate-in-line
    */
  def separate(n: Int, ctx: YamlContext): Boolean = ctx match {
    case FlowKey | BlockKey => separateInLine()
    case _ =>
      matches(multilineComment() && linePrefix(n, flow = true)) || separateInLine()
  }

  def separateFlow(n: Int, ctx: YamlContext): Boolean = ctx match {
    case FlowKey | BlockKey => separateInLine()
    case _ =>
      matches(multilineComment() && (linePrefix(n, flow = true) || flowLinePrefix)) || separateInLine()
  }

  /**
    * Directives are instructions to the YAML processor.
    * [82]	l-directive	::=	“%”
    * ( [[yamlDirective ns-yaml-directive]]
    * | [[tagDirective ns-tag-directive]]
    * | [[reservedDirective ns-reserved-directive]] )
    * s-l-comments
    */
  private def directive() =
    currentChar == '%' && emit(BeginDirective) && emitIndicator() && (
        matches(yamlDirective()) || matches(tagDirective()) || matches(reservedDirective())
    ) && emit(EndDirective) && multilineComment()

  /**
    * Each directive is specified on a separate non-indented line starting with the “%” indicator,
    * followed by the directive name and a list of parameters.
    * <blockquote><pre>
    *
    * [83]	ns-reserved-directive	::=	ns-directive-name ( s-separate-in-line ns-directive-parameter )*
    * [84]	ns-directive-name	    ::=	ns-char+
    * [85]	ns-directive-parameter	::=	ns-char+
    *
    * </blockquote></pre>
    */
  private def reservedDirective() = {
    def nameOrParameter =
      if (!isNsChar(currentChar)) false
      else {
        do consume() while (isNsChar(currentChar))
        emit(MetaText)
      }

    nameOrParameter && zeroOrMore(separateInLine() && nameOrParameter)
  }

  /**
    * [86]	ns-yaml-directive	::=	“Y” “A” “M” “L” s-separate-in-line ns-yaml-version	
    * [87]	ns-yaml-version	::=	ns-dec-digit+ “.” ns-dec-digit+
    */
  private def yamlDirective() =
    if (!consume("YAML")) false
    else {
      emit(MetaText)
      if (!separateInLine() || !isNsDecDigit(currentChar)) false
      else {
        consumeWhile(isNsDecDigit)
        if (currentChar != '.' || !isNsDecDigit(lookAhead(1))) false
        else {
          consume()
          consumeWhile(isNsDecDigit)
          emit(MetaText)
        }
      }
    }

  /**
    * The “TAG” directive establishes a tag shorthand notation for specifying node tags.
    * Each “TAG” directive associates a handle with a prefix.
    * This allows for compact and readable tag notation.<p>
    * [88]	ns-tag-directive	::=	“T” “A” “G”
    * [[separateInLine s-separate-in-line]]
    * [[tagHandle c-tag-handle]]
    * [[separateInLine s-separate-in-line]]
    * [[tagPrefix ns-tag-prefix]]
    */
  private def tagDirective() =
    if (!consume("TAG")) false
    else {
      emit(MetaText)
      separateInLine() && tagHandle() && separateInLine() && tagPrefix()
    }

  /**
    * The tag handle exactly matches the prefix of the affected tag shorthand.
    * There are three tag handle variants:<p>
    *
    * [89]	c-tag-handle	::=	  c-named-tag-handle | c-secondary-tag-handle | c-primary-tag-handle
    * [91]	c-secondary-tag-handle	::=	“!” “!”
    * [92]	c-named-tag-handle	::=	“!” ns-word-char+ “!”
    */
  private def tagHandle(): Boolean =
    emit(BeginHandle) && {
      // secondary tag handle
      lookAhead(1) == '!' && indicator('!') && indicator('!') ||
      // named tag handle
      isWordChar(lookAhead(1)) && matches { // named tag handle
        indicator('!')
        consumeWhile(isWordChar)
        emit(MetaText) && indicator('!')
      } ||
      indicator('!')
    } && emit(EndHandle)

  /**
    * There are two tag prefix variants:<p>
    * Local Tag Prefix:
    * If the prefix begins with a “!” character, shorthands using the handle are expanded to a local tag.<p>
    * Global Tag Prefix:
    * If the prefix begins with a character other than “!”, it must to be a valid URI prefix<p>
    * <blockquote><pre>
    *
    *
    * [93]	ns-tag-prefix	        ::=	c-ns-local-tag-prefix | ns-global-tag-prefix
    * [94]	c-ns-local-tag-prefix	::=	“!” ns-uri-char*
    * [95]	ns-global-tag-prefix	::=	ns-tag-char ns-uri-char*
    *
    * </blockquote></pre>
    */
  private def tagPrefix() = matches {
    emit(BeginTag) && {
      indicator('!') && zeroOrMore(uriChar()) || tagChar() && zeroOrMore(uriChar())
    } && emit(MetaText) && emit(EndTag)
  }

  /** Each node may have two optional properties, anchor and tag,
    * in addition to its content.
    * Node properties may be specified in any order before the node’s content.
    * Either or both may be omitted.<p>
    * <blockquote><pre>
    *
    * [96]	c-ns-properties(n,c)	::=
    * ( [[tagProperty c-ns-tag-property]]
    * ( [[separate s-separate(n,c)]] [[anchorProperty c-ns-anchor-property]] )? )
    * |  ( [[anchorProperty c-ns-anchor-property]]
    * ( [[separate s-separate(n,c)]] [[tagProperty c-ns-tag-property]] )? )
    * </blockquote></pre>
    */
  private def nodeProperties(n: Int, c: YamlContext) = matches {
    emit(BeginProperties) && (
        tagProperty() && optional(matches {
          val b1 = separate(n, c)
          b1 && anchorProperty()
        }) ||
        anchorProperty() && optional(matches(separate(n, c) && tagProperty()))
    ) &&
    emit(EndProperties)
  }

  /**
    * The tag property identifies the type of the native data structure presented by the node.
    * A tag is denoted by the “!” indicator.
    * <blockquote><pre>
    *
    * [97]	c-ns-tag-property	::=	  c-verbatim-tag | c-ns-shorthand-tag | c-non-specific-tag
    * [98]	c-verbatim-tag	    ::=	“!” “<” [[uriChar ns-uri-char]]+ “>”
    * [99]	c-ns-shorthand-tag	::=	[[tagHandle c-tag-handle]] [[tagChar ns-tag-char]]+
    * [100]	c-ns-shorthand-tag	::=	“!”
    *
    * </blockquote></pre>
    */
  private def tagProperty() =
    currentChar == '!' && emit(BeginTag) && {
      // c-ns-shorthand-tag
      lookAhead(1) == ' ' && indicator('!') ||
      matches {
        // c-verbatim-tag
        indicator('!') &&
        indicator('<') && oneOrMore(uriChar()) && emit(MetaText) && indicator('>')
      } ||
      matches {
        tagHandle() && (
            matches {
              tagChar() && {
                while (tagChar()) {}
                emit(MetaText)
              }
            } ||
            emit(Error)
        )
      }
    } &&
      emit(EndTag)

  /**
    * An anchor is denoted by the “&” indicator.
    * It marks a node for future reference.
    * An alias node can then be used to indicate additional inclusions of the anchored node.
    * An anchored node need not be referenced by any alias nodes; in particular, it is valid for all nodes to be anchored.<p>
    *
    * [101]	c-ns-anchor-property	::=	“&” [[anchorName ns-anchor-name]]
    *
    * </blockquote></pre>
    *
    */
  @failfast private def anchorProperty() =
    currentChar == '&' && isNsAnchorChar(lookAhead(1)) &&
      emit(BeginAnchor) && emitIndicator() && anchorName() && emit(EndAnchor)

  /** [102]	ns-anchor-char	::=	[[isNsChar ns-char]] - [[isFlowIndicator c-flow-indicator]] */
  private def isNsAnchorChar(c: Int) = isNsChar(c) && !isFlowIndicator(c)

  /**
    * An anchor name<p>
    * [103]	ns-anchor-name	::=	[[isNsAnchorChar ns-anchor-char]]+
    */
  private def anchorName() = {
    consumeWhile(isNsAnchorChar)
    emit(MetaText)
  }

  /**
    * An alias node is denoted by the “*” indicator.
    * The alias refers to the most recent preceding node having the same anchor.<p>
    *
    * [104]	c-ns-alias-node	::=	“*” [[anchorName ns-anchor-name]]
    *
    */
  @failfast private def aliasNode() =
    currentChar == '*' && isNsAnchorChar(lookAhead(1)) &&
      emit(BeginAlias) && emitIndicator() && anchorName() && emit(EndAlias)

  /**
    * [105]	e-scalar	::=	/* Empty */
    */
  def emptyScalar(n :Int): Boolean = {
    emit(BeginScalar)
    matches(lineBreakSequence()>0 && breakNonContent() && currentSpaces > n && indent(currentSpaces) && breakComment())
    emit(EndScalar)
  }

  /**
    * [106] e-node :: e-scalar
    */
  private def emptyNode(n:Int) = {
    emit(BeginNode)
    emptyScalar(n)
    emit(EndNode)
  }

  private def emptyValue(n:Int) = {
    emit(BeginNode)
    emit(BeginScalar)
    matches(lineBreakSequence()>0 && breakNonContent() && currentSpaces > n && indent(currentSpaces) && breakComment()) || matches(separateInLine())
    emit(EndScalar)
    emit(EndNode)
  }
  /**
    * Process either simple or double quoted scalars
    */
  private def quotedScalar(n: Int, c: YamlContext, quoteChar: Char) = currentChar == quoteChar && {
    var inText = false

    def emitText(): Unit = if (inText) {
      emit(Text); inText = false
    }

    def allHexa(chars: Int) = {
      val i = countWhile(n => isNsHexDigit(lookAhead(n)))
      i >= chars
    }

    def processHexa(chars: Int) = emitIndicator() && consumeAndEmit(chars, if (allHexa(chars)) MetaText else Error)

    emit(BeginScalar)
    emitIndicator()
    var done = false

    while (!done) {
      currentChar match {
        case '"' if quoteChar == '"' =>
          done = true
        case '\'' if quoteChar == '\'' =>
          if (lookAhead(1) != '\'') done = true
          else {
            emitText()
            emit(BeginEscape)
            emitIndicator()
            consumeAndEmit(MetaText)
            emit(EndEscape)
          }

        case '\r' | '\n' =>
          if (c != BlockKey && c != FlowKey) {
            emitText()
            matches(breakNonContent() && emptyLine(n, c)) || consumeAndEmit(LineFold)
            indent(n)
            consumeAndEmit(input.countWhiteSpaces(), WhiteSpace)
          }
          else {
            inText = false
            emit(Error)
            done = true
          }
        case '\\' if quoteChar == '"' =>
          emitText()
          emit(BeginEscape)
          emitIndicator()
          val nl = currentChar == '\n'
          if (nl) breakNonContent()
          else
            escapeSeq(currentChar) match {
              case -1 =>
                consumeAndEmit(Error)
              case 'x' => processHexa(2)
              case 'u' => processHexa(4)
              case 'U' => processHexa(8)
              case _ =>
                consumeAndEmit(MetaText)
            }
          emit(EndEscape)
          if (nl) consumeAndEmit(input.countWhiteSpaces(), WhiteSpace)

        case ' ' | '\t' =>
          val spaces = input.countWhiteSpaces()
          if (isBBreak(lookAhead(spaces))) {
            emitText()
            consumeAndEmit(spaces, WhiteSpace)
          }
          else {
            inText = true
            consume(spaces)
          }

        case EofChar =>
          emit(Error)
          done = true

        case _ =>
          inText = true
          consume()
      }
    }
    emitText()
    emitIndicator()
    emit(EndScalar)
  }

  /** todo deprecate */
  def isPlainFirst(chr: Int, nextChar: Int, ctx: YamlContext): Boolean =
    isNsChar(chr) && !isIndicator(chr) || (chr == '-' || chr == ':' || chr == '?') && isPlainSafe(nextChar, ctx)

  /**
    * [126]	ns-plain-first(c) ::=	  ( ns-char - c-indicator )
    * |   ( ( “?” | “:” | “-” )    Followed by an ns-plain-safe(c))
    */
  @failfast private def plainFirst(ctx: YamlContext): Boolean = {
    val chr = currentChar
    val result = isNsChar(chr) && !isIndicator(chr) || (chr == '-' || chr == ':' || chr == '?') && isPlainSafe(
        lookAhead(1),
        ctx)
    if (result) consume()
    result
  }

  /**
    * [127]	ns-plain-safe(c)	::=	c = flow-out  ⇒ ns-plain-safe-out
    * c = flow-in   ⇒ ns-plain-safe-in
    * c = block-key ⇒ ns-plain-safe-out
    * c = flow-key  ⇒ ns-plain-safe-in
    * [128]	ns-plain-safe-out	::=	ns-char
    * [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
    */
  private def isPlainSafe(chr: Int, ctx: YamlContext): Boolean = ctx match {
    case FlowIn | FlowKey => isNsChar(chr) && !isFlowIndicator(chr)
    case _                => isNsChar(chr)
  }

  /**
    * [130]	ns-plain-char(c)	::=	  ( ns-plain-safe(c) - “:” - “#” )
    * |   ( /* An ns-char preceding */ “#” )
    * |   ( “:” /* Followed by an ns-plain-safe(c) */ )
    */
  def isPlainChar(offset: Int, ctx: YamlContext): Boolean = lookAhead(offset) match {
    case '#'                                => lookAhead(offset - 1) != ' '
    case ':'                                => isPlainSafe(lookAhead(offset + 1), ctx)
    case ' ' | '\t' | '\r' | '\n' | EofChar => false
    case _                                  => isPlainSafe(lookAhead(offset), ctx)
  }

  private def plainChar(ctx: YamlContext): Boolean = {
    val result = isPlainChar(0, ctx)
    if (result) consume()
    result
  }

  /**
    *
    * [131]	ns-plain(n,c)	::=	c = flow-out  ⇒ ns-plain-multi-line(n,c)
    * c = flow-in   ⇒ ns-plain-multi-line(n,c)
    * c = block-key ⇒ ns-plain-one-line(c)
    * c = flow-key  ⇒ ns-plain-one-line(c)
    */
  def plainScalar(n: Int, ctx: YamlContext): Boolean = {
    emit(BeginScalar)
    ctx match {
      case FlowKey | BlockKey => plainOneLine(ctx)
      case _                  => plainMultiline(n, ctx)
    }
    emit(EndScalar)
  }

  /**
    * [132]	nb-ns-plain-in-line(c)	::=	( s-white* ns-plain-char(c) )*
    */
  @failfast private def plainInLine(ctx: YamlContext): Boolean = {
    while ({
      val spaces = input.countWhiteSpaces()
      if (!isPlainChar(spaces, ctx)) false
      else {
        consume(spaces + 1)
        true
      }
    }) {}
    true
  }

  /** [133]	ns-plain-one-line(c)	::=	ns-plain-first(c) nb-ns-plain-in-line(c) */
  @failfast private def plainOneLine(ctx: YamlContext): Unit = {
    plainFirst(ctx)
    plainInLine(ctx)
    emit(Text)
  }

  /**
    * [134]	s-ns-plain-next-line(n,c)	::=	s-flow-folded(n) ns-plain-char(c) nb-ns-plain-in-line(c)
    * [135]	ns-plain-multi-line(n,c)	::=	ns-plain-one-line(c) s-ns-plain-next-line(n,c)*
    */
  private def plainMultiline(n: Int, ctx: YamlContext): Unit = {
    plainOneLine(ctx)
    zeroOrMore({
      if (!flowFolded(n)) false
      else {
        if (!plainChar(ctx)) false
        else {
          val b3 = plainInLine(ctx)
          if (b3) emit(Text)
          b3
        }
      }

    })
  }

  /**
    * Flow sequence content is denoted by surrounding “[” and “]” characters.<p>
    *
    * <blockquote><pre>
    * [136]	in-flow(c)	::=
    * c = flow-out  ⇒ flow-in
    * c = flow-in   ⇒ flow-in
    * c = block-key ⇒ flow-key
    * c = flow-key  ⇒ flow-key
    */
  private def inFlow(ctx: YamlContext) =
    if (ctx == BlockKey || ctx == FlowKey) FlowKey else FlowIn

  /**
    * Flow sequence content is denoted by surrounding “[” and “]” characters.<p>
    *
    * [137]	c-flow-sequence(n,c)	::=	“[”
    * [[separate s-separate(n,c)]]?
    * [\[flowSequenceEntries ns-s-flow-seq-entries(n,in-flow(c))]\]?
    * “]”
    */
  def flowSequence(n: Int, ctx: YamlContext): Boolean = currentChar == '[' && matches {
    emit(BeginSequence) && emitIndicator() &&
    optional(separateFlow(n: Int, ctx)) &&
    optional(flowSequenceEntries(n, inFlow(ctx))) &&
    currentChar == ']' && emitIndicator() && emit(EndSequence)
  }

  /**
    * Sequence entries are separated by a “,” character.<p>
    *
    * [138]	ns-s-flow-seq-entries(n,c)	::=	[[flowSequenceEntry ns-flow-seq-entry(n,c)]]
    * [[separate s-separate(n,c)]]?
    * ( “,”
    * [[separate s-separate(n,c)]]?
    * [[flowSequenceEntries ns-s-flow-seq-entries(n,c)]]?
    * )?
    */
  @tailrec private def flowSequenceEntries(n: Int, ctx: YamlContext): Boolean = currentChar != ']' && {

    flowSequenceEntry(n, ctx) && {
      def isInvalid(chr: Int) = chr != ']' && chr != ',' && chr != EofChar

      separateFlow(n, ctx)
      if (isInvalid(currentChar)) {
        consumeWhile(isInvalid)
        emit(Error)
      }
      val c = currentChar
      if (c == ']' || c == EofChar) true
      else {
        emitIndicator()
        separateFlow(n, ctx)
        flowSequenceEntries(n, ctx)
      }
    }
  }

  /**
    * Any flow node may be used as a flow sequence entry.
    * In addition, YAML provides a compact notation
    * for the case where a flow sequence entry is a mapping with a single key: value pair.<p>
    *
    * [139]	ns-flow-seq-entry(n,c)	::=	[\[flowPair ns-flow-pair(n,c)]\]
    * | [\[flowNode ns-flow-node(n,c)]\]
    */
  def flowSequenceEntry(n: Int, ctx: YamlContext): Boolean = {
    matches(
        emit(BeginNode) && emit(BeginMapping, BeginPair) &&
          flowPair(n, ctx) &&
          emit(EndPair, EndMapping) && emit(EndNode)) ||
    matches(emit(BeginNode) && flowNode(n, ctx) && emit(EndNode))
  }

  /**
    * Flow mappings are denoted by surrounding “{” and “}” characters.<p>
    *
    * [140]	c-flow-mapping(n,c)	::=	“{”
    * [[separate s-separate(n,c)]]?
    * [[flowMapEntries ns-s-flow-map-entries(n,in-flow(c))]]?
    * “}”
    */
  private def flowMapping(n: Int, c: YamlContext) = currentChar == '{' && matches {
    emit(BeginMapping) && emitIndicator() &&
    optional(separateFlow(n: Int, c)) &&
    optional(flowMapEntries(n, inFlow(c))) &&
    currentChar == '}' && emitIndicator() && emit(EndMapping) && optional(separateInLine())
  }

  /**
    * Mapping entries are separated by a “,” character.<p>
    *
    * [141]	ns-s-flow-map-entries(n,c)	::=	ns-flow-map-entry(n,c) s-separate(n,c)?
    * ( “,” s-separate(n,c)? ns-s-flow-map-entries(n,c)? )?
    */
  @tailrec private def flowMapEntries(n: Int, ctx: YamlContext): Boolean = currentChar != '}' && {
    flowMapEntry(n, ctx) && {
      separateFlow(n, ctx)
      indicator(',') && {
        separateFlow(n, ctx)
        flowMapEntries(n, ctx)
      }
    }
  }

  /**
    * A Flow Map Entry<p>
    * If the optional “?” mapping key indicator is specified, the rest of the entry may be completely empty.<p>
    *
    * <blockquote><pre>
    * [142]	ns-flow-map-entry(n,c)	::=	  ( “?” [[separate s-separate(n,c)]]
    * [[flowMapExplicitEntry ns-flow-map-explicit-entry(n,c)]]
    * )
    * | [[flowMapImplicitEntry ns-flow-map-implicit-entry(n,c)]]
    *
    * </blockquote></pre>
    */
  private def flowMapEntry(n: Int, c: YamlContext) =
    emit(BeginPair) && {
      indicator('?') && separate(n, c) && flowMapExplicitEntry(n, c) ||
      flowMapImplicitEntry(n, c)
    } && emit(EndPair)

  /**
    * [143]	ns-flow-map-explicit-entry(n,c)	::=	  [[flowMapImplicitEntry ns-flow-map-implicit-entry(n,c)]]
    * |   ( [[emptyNode e-node]] [[emptyNode e-node]])
    */
  private def flowMapExplicitEntry(n: Int, c: YamlContext) = {
    (currentChar == ',' || currentChar == '}') && emptyNode(n) && emptyNode(n) || flowMapImplicitEntry(n, c)
  }

  /**
    * <blockquote><pre>
    * [144]	ns-flow-map-implicit-entry(n,c)	   ::=	ns-flow-map-yaml-key-entry(n,c)
    * |  [[flowMapEmptyKeyEntry c-ns-flow-map-empty-key-entry(n,c)]]
    * |  [[flowMapJsonKeyEntry c-ns-flow-map-json-key-entry(n,c)]]
    *
    * [145]	ns-flow-map-yaml-key-entry(n,c)	   ::=	[[flowYamlNode ns-flow-yaml-node(n,c)]]
    * ( ([[separate s-separate(n,c)]]?
    * [[flowMapSeparateValue c-ns-flow-map-separate-value(n,c)]])
    * |  [[emptyNode e-node]]
    * )
    *
    * </blockquote></pre>
    * // todo complete
    */
  private def flowMapImplicitEntry(n: Int, c: YamlContext) =
    matches {
      val b1 = emit(BeginNode) && flowYamlNode(n, c)
      b1 && emit(EndNode) && (
          matches {
            optional(separate(n, c)) && flowMapSeparateValue(n, c)
          } || emptyNode(n)
      )
    } || flowMapEmptyKeyEntry(n, c) || flowMapJsonKeyEntry(n, c)

  /**
    * [146]	c-ns-flow-map-empty-key-entry(n,c)	::=	[[emptyNode e-node]]
    * [[flowMapSeparateValue c-ns-flow-map-separate-value(n,c)]]
    */
  private def flowMapEmptyKeyEntry(n: Int, c: YamlContext) = matches {
    emptyNode(n) && flowMapSeparateValue(n, c)
  }

  /**
    * [147]	c-ns-flow-map-separate-value(n,c)	::=	“:” (Not followed by an [[isPlainSafe ns-plain-safe(c)]] )
    * ( ( [[separate s-separate(n,c)]] [[flowNode ns-flow-node(n,c)]])
    * | [[emptyNode() e-node]]
    * )
    */
  private def flowMapSeparateValue(n: Int, c: YamlContext) =
    currentChar == ':' && !isPlainSafe(lookAhead(1), c) && emitIndicator() && (matches {
      separate(n, c)
      emit(BeginNode)
      flowNode(n, c) &&
      emit(EndNode)
    } ||
      emptyNode(n))

  /**
    * [148]	c-ns-flow-map-json-key-entry(n,c)	::=	[[flowJsonNode c-flow-json-node(n,c)]]
    * ( ( [[separate s-separate(n,c)]]?
    * [[flowMapAdjacentValue c-ns-flow-map-adjacent-value(n,c)]]
    * )
    * | [[emptyNode e-node]]
    * )
    */
  private def flowMapJsonKeyEntry(n: Int, c: YamlContext) = matches {
    flowJsonNode(n, c) && (matches {
      separate(n, c)
      flowMapAdjacentValue(n, c)
    } || emptyNode(n))
  }

  /**
    * [149]	c-ns-flow-map-adjacent-value(n,c)	::=	“:” ( ( [[separate s-separate(n,c)]]?
    * [[flowNode ns-flow-node(n,c)]] )
    * | [[emptyNode e-node]]
    * )
    */
  private def flowMapAdjacentValue(n: Int, c: YamlContext) = indicator(':') && (
      matches {
        separate(n, c)
        emit(BeginNode)
        flowNode(n, c) && emit(EndNode)
      } || emptyNode(n)
  )

  /**
    * If the “?” indicator is explicitly specified, parsing is unambiguous,
    * and the syntax is identical to the general case. <p>
    *
    * [150]	ns-flow-pair(n,c)	::=	  ( “?” [[separate s-separate(n,c)]]
    * [[flowMapExplicitEntry ns-flow-map-explicit-entry(n,c)]]
    * )
    * | [[flowPairEntry ns-flow-pair-entry(n,c)]]
    */
  private def flowPair(n: Int, c: YamlContext): Boolean =
    indicator('?') && separate(n, c) && flowMapExplicitEntry(n, c) || flowPairEntry(n, c)

  /**
    * <blockquote><pre>
    * [151]	ns-flow-pair-entry(n,c)	::=	  ns-flow-pair-yaml-key-entry(n,c)
    * | [[flowMapEmptyKeyEntry c-ns-flow-map-empty-key-entry(n,c)]]
    * | c-ns-flow-pair-json-key-entry(n,c)
    *
    * [152]	ns-flow-pair-yaml-key-entry(n,c)	::=	[[implicitYamlKey ns-s-implicit-yaml-key(flow-key)]]
    * [[flowMapSeparateValue c-ns-flow-map-separate-value(n,c)]]
    *
    * [153]	c-ns-flow-pair-json-key-entry(n,c)	::=	[[implicitJsonKey c-s-implicit-json-key(flow-key)]]
    * [[flowMapAdjacentValue c-ns-flow-map-adjacent-value(n,c)]]
    *
    * </blockquote></pre>
    */
  private def flowPairEntry(n: Int, c: YamlContext) =
    matches {
      val b1 = implicitYamlKey(FlowKey)
      b1 && flowMapSeparateValue(n, c)
    } ||
      flowMapEmptyKeyEntry(n, c) ||
      matches(implicitJsonKey(FlowKey) && flowMapAdjacentValue(n, c))

  /** [154]	implicit-yaml-key(c)	::=	ns-flow-yaml-node(n/a,c) s-separate-in-line? */
  def implicitYamlKey(ctx: YamlContext): Boolean =
    emit(BeginNode) && flowYamlNode(0, ctx) && emit(EndNode) && optional(separateInLine())

  /** [155]	c-s-implicit-json-key(c)	::=	c-flow-json-node(n/a,c) s-separate-in-line? */
  def implicitJsonKey(ctx: YamlContext): Boolean = flowJsonNode(0, ctx) && optional(separateInLine())

  /**
    * [156] ns-flow-yaml-content(n,c)	::=	ns-plain(n,c)
    */
  @failfast private def flowYamlContent(n: Int, ctx: YamlContext): Boolean =
    isPlainFirst(currentChar, lookAhead(1), ctx) && plainScalar(n, ctx)

  /**
    * JSON-like flow styles all have explicit start and end indicators.<p>
    * <blockquote><pre>
    * [157] c-flow-json-content(n,c)	::=
    * [[flowSequence c-flow-sequence(n,c)]]
    * | [[flowMapping c-flow-mapping(n,c)]]
    * | [[quotedScalar c-single-quoted(n,c)]]
    * | [[quotedScalar c-double-quoted(n,c)]]
    * </blockquote></pre>
    */
  @failfast def flowJsonContent(n: Int, ctx: YamlContext): Boolean = currentChar match {
    case '['  => flowSequence(n, ctx)
    case '{'  => flowMapping(n, ctx)
    case '\'' => quotedScalar(n, ctx, '\'')
    case '"'  => quotedScalar(n, ctx, '"')
    case _    => false
  }

  /**
    * [158] ns-flow-content(n,c)	    ::=	ns-flow-yaml-content(n,c) | c-flow-json-content(n,c)
    */
  def flowContent(n: Int, ctx: YamlContext): Boolean = flowJsonContent(n, ctx) || flowYamlContent(n, ctx)

  /**
    * [159]	ns-flow-yaml-node(n,c)	::=	  [\[aliasNode c-ns-alias-node]\]
    * |   [\[flowYamlContent ns-flow-yaml-content(n,c)]\]
    * | ( [\[nodeProperties c-ns-properties(n,c)]\]
    * ( ( [[separate s-separate(n,c)]] [\[flowYamlContent ns-flow-yaml-cont(n,c)]\] )
    * | [[emptyScalar e-scalar]] )
    * )
    */
  def flowYamlNode(n: Int, c: YamlContext): Boolean =
    aliasNode() ||
      flowYamlContent(n, c) ||
      nodeProperties(n, c) && (matches(separate(n, c) && flowYamlContent(n, c)) || emptyScalar(n))

  /**
    * [160]	c-flow-json-node(n,c)	::=	( c-ns-properties(n,c) s-separate(n,c) )? c-flow-json-content(n,c)
    */
  def flowJsonNode(n: Int, ctx: YamlContext): Boolean =
    emit(BeginNode) &&
      optional(nodeProperties(n, ctx) && separate(n, ctx)) &&
      flowJsonContent(n, ctx) &&
      emit(EndNode)

  /**
    * A complete flow node also has optional node properties,
    * except for alias nodes which refer to the anchored node properties.<p>
    *
    * [161] ns-flow-node(n,c)	::=	  [[aliasNode c-ns-alias-node]]
    * |    [[flowContent ns-flow-content(n,c)]]
    * |  ( [[nodeProperties c-ns-properties(n,c)]]
    * ( ( [[separate s-separate(n,c)]] [[flowContent ns-flow-content(n,c) )]])
    * | [[emptyScalar e-scalar]]
    * )
    */
  private def flowNode(n: Int, c: YamlContext): Boolean = {
    aliasNode() ||
    matches(flowContent(n, c)) ||
    nodeProperties(n, c) &&
    (matches(separate(n, c) && flowContent(n, c)) || emptyScalar(n))
  }

  /**
    * Returns the new defined indentation or MAX_VALUE if autodetect
    * <blockquote><pre>
    * [162] c-b-block-header(m,t)	::=
    * ( ( c-indentation-indicator(m) c-chomping-indicator(t) )
    * | ( c-chomping-indicator(t) c-indentation-indicator(m) )
    * )
    * s-b-comment
    *
    * [163] c-indentation-indicator(m) ::=	ns-dec-digit ⇒ m = ns-dec-digit - #x30
    * | Empty
    *
    * [164] c-chomping-indicator(t) ::=
    * “-” ⇒ t = strip
    * |   “+” ⇒ t = keep
    * |   “ ” ⇒ t = clip
    *
    * </blockquote></pre>
    */
  def blockHeader(n: Int): (Int, Char) = {
    def chompingIndicator = currentChar match {
      case '+' =>
        emitIndicator()
        '+'
      case '-' =>
        emitIndicator()
        '-'
      case _ => ' '
    }

    var t = chompingIndicator

    val c = currentChar
    var m =
      if (!isNsDecDigit(c) || c == '0') -1
      else {
        emitIndicator()
        c - '0'
      }
    if (t == ' ') t = chompingIndicator

    if (!spaceBreakComment()) {
      error()
      breakComment()
    }
    if (m == -1) { // Autodetect
      // Skip Empty Lines
      val s = saveState
      zeroOrMore(emptyLine(n, BlockIn))
      val n1 = input.countSpaces()
      m = 1 max (n1 - n)
      restoreState(s)
    }
    (m, t)
  }

  /**
    * Process the last block of an Scalar based on the chomp indicator
    *
    * <blockquote><pre>
    *
    * [165]	b-chomped-last(t) ::=
    * t = strip(-) ⇒ b-non-content  | EOF
    * t = clip     ⇒ b-as-line-feed | EOF
    * t = keep (+) ⇒ b-as-line-feed | EOF
    * </blockquote></pre>
    */
  def chompedLast(t: Char): Boolean =
    matches(currentChar == EofChar && t == '-' && emit(EndScalar)) ||
      (currentChar == EofChar || t != '-' && breakAsLineFeed() || emit(EndScalar) && breakNonContent())

  /**
    * <blockquote><pre>
    *
    * [166]	l-chomped-empty(n,t) ::=
    * t = strip(-) ⇒ l-strip-empty(n)
    * t = clip     ⇒ l-strip-empty(n)
    * t = keep (+) ⇒ l-keep-empty(n)
    *
    * [167]	l-strip-empty(n)	::=	([[indentLessOrEqual s-indent(≤n)]] [[breakNonContent b-non-content]])*
    * [[trailComments l-trail-comments(n)]]?
    *
    * [168]	l-keep-empty(n)	    ::=	[[emptyLine l-empty(n,block-in)]]*
    * [[trailComments l-trail-comments(n)]]?
    * </blockquote></pre>
    */
  def chompedEmpty(n: Int, t: Char): Boolean = {
    if (t != '+') {
      if (t != '-') emit(EndScalar)
      zeroOrMore(indentLessOrEqual(n) && breakNonContent()) //strip empty
    }
    else {
      zeroOrMore(emptyLine(n, BlockIn)) // keep empty
      emit(EndScalar)
    }
    trailComments(n)
    true
  }

  /**
    * Explicit comment lines may follow the trailing empty lines.
    * To prevent ambiguity, the first such comment line must be less indented than the block scalar content.
    * Additional comment lines, if any, are not so restricted.
    * This is the only case where the indentation of comment lines is constrained.<p>
    *
    * [169]	l-trail-comments(n)	::=	[[indentLess s-indent(&lt;n)]]
    * [[commentText c-nb-comment-text b-comment]]
    * [[lineComment l-comment]]*
    */
  def trailComments(n: Int): Boolean = matches(indentLess(n) && commentText() && zeroOrMore(lineComment()))

  /**
    * [170]	c-l+literal(n)	::=	 “|” [[blockHeader c-b-block-header(m,t)]] [[literalContent l-literal-content(n+m,t)]]
    *
    */
  def literalScalar(n: Int): Boolean =
    if (currentChar != '|') false
    else {
      emit(BeginScalar)
      emitIndicator()
      val (m, t) = blockHeader(n)
      literalContent(n + m, t)
    }

  /**
    * [171]	l-nb-literal-text(n)	::=	l-empty(n,block-in)* s-indent(n) nb-char+
    */
  private def literalContentLine(n: Int) = nonEndOfDocument && {
    zeroOrMore(emptyLine(n, BlockIn))
    indent(n) && !isBreakOrEof(currentChar) && {
      consumeWhile(!isBreakOrEof(_))
      emit(Text)
    }
  }

  /**
    * <blockquote><pre>
    * [173]	l-literal-content(n,t)	::=
    * ( [[literalContentLine l-nb-literal-text(n)]]
    * b-nb-literal-next(n) *
    * [[chompedLast b-chomped-last(t)]]
    * )?
    * [[chompedEmpty l-chomped-empty(n,t)]]
    *
    * [172]	b-nb-literal-next(n) ::= [[breakAsLineFeed b-as-line-feed]] [[literalContentLine l-nb-literal-text(n)]]
    *
    * </blockquote></pre>
    */
  def literalContent(n: Int, t: Char): Boolean = {
    matches {
      literalContentLine(n) && zeroOrMore(breakAsLineFeed() && literalContentLine(n)) &&
      chompedLast(t)
    } || t == '-' && emit(EndScalar)
    chompedEmpty(n, t)
  }

  /**
    * The folded style is denoted by the “>” indicator.
    * It is similar to the literal style; however, folded scalars are subject to line folding.<p>
    * [174]	c-l+folded(n)	::=	“>” [[blockHeader c-b-block-header(m,t)]]
    * [\[foldedContent l-folded-content(n+m,t)]\]
    */
  def foldedScalar(n: Int): Boolean = currentChar == '>' && emit(BeginScalar) && emitIndicator() && {
    val (m, t) = blockHeader(n)
    foldedContent(n + m, t)
  }

  /**
    * Folding allows long lines to be broken anywhere a single space character separates two non-space characters.<p>
    * [175]	s-nb-folded-text(n)	  ::=	[[indent s-indent(n)]] [[isNsChar ns-char]] [[isNBChar nb-char]]*
    */
  private def foldedText(n: Int) =
    if (input.countSpaces(0, MAX_VALUE) != n || !isNsChar(lookAhead(n))) false
    else {
      consumeAndEmit(n, Indent)
      consumeWhile(isNBChar)
      emit(Text)
    }

  /**
    * [176]	l-nb-folded-lines(n)  ::=	[[foldedText s-nb-folded-text(n)]]
    * ([[folded b-l-folded(n,block-in)]] [[foldedText s-nb-folded-text(n)]] )*
    */
  private def foldedLines(n: Int) = {
    val b1 = foldedText(n)
    b1 && zeroOrMore(folded(n, BlockIn) && foldedText(n))
  }

  /**
    * Lines starting with white space characters (more-indented lines) are not folded.<p>
    * [177]	s-nb-spaced-text(n)	::=	[[indent s-indent(n)]] [[isWhite s-white]] [[isNBChar nb-char]]*
    */
  private def spacedText(n: Int): Boolean = {
    val m = input.countSpaces(0, MAX_VALUE)
    if (m < n || m == n && !isWhite(lookAhead(n))) false
    else {
      consumeAndEmit(n, Indent)
      consumeWhile(isNBChar)
      emit(Text)
    }
  }

  /**
    * <blockquote><pre>
    * [178]	b-l-spaced(n)	::=	[[breakAsLineFeed b-as-line-feed]] [[emptyLine l-empty(n,block-in)]]*
    * [179]	l-nb-spaced-lines(n)	::=	[[spacedText s-nb-spaced-text(n)]]
    * ( b-l-spaced(n) [[spacedText s-nb-spaced-text(n)]] )*
    * </blockquote></pre>
    */
  private def spacedLines(n: Int) = {
    val b = spacedText(n)
    b && zeroOrMore(breakAsLineFeed() && zeroOrMore(emptyLine(n, BlockIn)) && spacedText(n))
  }

  /**
    * Line breaks and empty lines separating folded and more-indented lines are also not folded.<p>
    * [180]	l-nb-same-lines(n)	::=	[[emptyLine l-empty(n,block-in)]]*
    * ( [[foldedLines l-nb-folded-lines(n)]]
    * | [[spacedLines l-nb-spaced-lines(n)]]
    * )
    */
  private def sameLines(n: Int) = nonEndOfDocument && {
    zeroOrMore(emptyLine(n, BlockIn))
    matches(foldedLines(n)) || matches(spacedLines(n))
  }

  /**
    * The final line break, and trailing empty lines if any, are subject to chomping and are never folded.<p>
    * <blockquote><pre>
    * [181]	l-nb-diff-lines(n)	    ::=	[[sameLines l-nb-same-lines(n)]]
    * ( [[breakAsLineFeed b-as-line-feed]] [[sameLines l-nb-same-lines(n)]])*
    *
    * [182]	l-folded-content(n,t)	::=	( l-nb-diff-lines(n) b-chomped-last(t) )? l-chomped-empty(n,t)
    *
    * </blockquote></pre>
    */
  private def foldedContent(n: Int, t: Char): Boolean = {
    matches {
      sameLines(n) && zeroOrMore(breakAsLineFeed() && sameLines(n)) &&
      chompedLast(t)
    } || t == '-' && emit(EndScalar)
    chompedEmpty(n, t)
  }

  /**
    * A block sequence is simply a series of nodes, each denoted by a leading “-” indicator.
    * The “-” indicator must be separated from the node by white space.
    * This allows “-” to be used as the first character
    * in a plain scalar if followed by a non-space character (e.g. “-1”).<p>
    *
    * <blockquote><pre>
    * [183]	l+block-sequence(n)	::=	( [[indent s-indent(n+m)]] [[blockSeqEntry c-l-block-seq-entry(n+m)]] )+
    * (For some fixed auto-detected m > 0)
    * </blockquote></pre>
    */
  def blockSequence(n: Int): Boolean = beginOfLine && {
    val m: Int = detectSequenceStart(n)
    m > 0 &&
    emit(BeginSequence) &&
    oneOrMore {
      val b1 = indent(n + m)
      b1 && blockSeqEntry(n + m)
    } &&
    emit(EndSequence)
  }

  private def detectSequenceStart(n: Int) = {
    val spaces = input.countSpaces(0, MAX_VALUE)
    if (lookAhead(spaces) == '-' && !isNsChar(lookAhead(spaces + 1))) spaces - n else 0
  }

  /**
    * [184]	c-l-block-seq-entry(n)	::=	“-” (Not followed by an ns-char)
    * [[blockIndented s-l+block-indented(n,block-in)]]
    * </blockquote></pre>
    */
  private def blockSeqEntry(n: Int) = indicator('-') && blockIndented(n, BlockIn)

  /**
    * The entry node may be either completely empty, be a nested block node, or use a compact in-line notation.<p>
    * The compact notation may be used when the entry is itself a nested block collection.<p>
    *
    * <blockquote><pre>
    * [185]	s-l+block-indented(n,c)	 ::=
    * ([[indent s-indent(m)]]
    * ( [[compactSequence ns-l-compact-sequence(n+1+m)]]
    * | [[compactMapping  ns-l-compact-mapping(n+1+m)]]
    * )
    * )
    * | [[blockNode s-l+block-node(n,c) ]]
    * | ( [[emptyNode e-node]] [[multilineComment s-l-comments]] )
    *
    * </blockquote></pre>
    */
  private def blockIndented(n: Int, ctx: YamlContext) = {
    {
      val m = detectSequenceStart(n) + n
      m > 0 && matches {
        indent(m) && emit(BeginNode) && compactSequence(n + 1 + m) && emit(EndNode)
      }
    } || {
      val m = detectMapStart(n) + n
      m > 0 && matches {
        indent(m) && emit(BeginNode) && compactMapping(n + 1 + m) && emit(EndNode)
      }
    } || {
      blockNode(n, ctx) ||
      matches(emptyNode(n) && multilineComment())
    }
  }

  /**
    * [186]	ns-l-compact-sequence(n) ::=	[[blockSeqEntry c-l-block-seq-entry(n)]]
    * ( [[indent s-indent(n)]] [[blockSeqEntry c-l-block-seq-entry(n)]] )*
    */
  private def compactSequence(n: Int): Boolean =
    emit(BeginSequence) && blockSeqEntry(n) && zeroOrMore(indent(n) && blockSeqEntry(n)) && emit(EndSequence)

  private def detectMapStart(n: Int) = {
    val spaces = input.countSpaces(0, MAX_VALUE)
    if (spaces > n && lookAhead(spaces) == '?' || lineContainsMapIndicator()) spaces - n else 0
  }

  /**
    * A Block mapping is a series of entries, each presenting a key: value pair.<p>
    * [187]	l+block-mapping(n)	    ::= ( [[indent s-indent(n+m)]]
    * [[blockMapEntry ns-l-block-map-entry(n+m)]]
    * )+
    * <p> For some fixed auto-detected m greater than 0
    */
  def blockMapping(n: Int): Boolean = beginOfLine && matches {
    emit(BeginMapping) && oneOrMoreEntries(n) && emit(EndMapping)
  }

  private def currentSpaces:Int =  input.countSpaces(0, MAX_VALUE)
  /**
    * Build entries from a parent indentation. Check for not valid entries and add those lines as errors
    * Try to add all entries between errors until the indentation returns to his parent level.
    * if a line is indented less or more than the first valid map entry, it will be considered error
    *
    * returns true if at least one entry was emitted
    * */
  def oneOrMoreEntries(parent:Int):Boolean = {

    zeroOrMore(!lineIsMapEntry() && invalidEntry(parent))
    var oneEntry = false
    var entryIndentation:Int = -1
    while(isIndented(parent) && notDocument(parent)){
      val start = detectMapStart(parent)
      if(entryIndentation == -1) entryIndentation = start
      if(oneOrMore(entryInParent(entryIndentation,start, parent))) oneEntry = true
      zeroOrMore(invalidEntry(parent))
    }
    oneEntry
  }

  private def lineIsMapEntry(): Boolean = {
    val next = lookAhead(currentSpaces)
    // do exists a way to know that a node matched against some valid root or value in a map??
    next == '?' || next == '{' || next == '[' || next == '|' || lineContainsMapIndicator()
  }

  private def isIndented(parent:Int) :Boolean= {
    currentChar != EofChar && (
      (parent < 0 && currentSpaces >= 0) ||
        (parent >= 0 && currentSpaces > parent))
  }

  private def entryInParent(indentation:Int, start:Int, parent:Int):Boolean =
    indentation == start && start > 0 && indent(parent + start) && blockMapEntry(parent + start)

  private def invalidEntry(parent:Int): Boolean = isIndented(parent) &&  errorMapLine(currentSpaces, parent)

  private def notDocument(parent:Int): Boolean = !(parent < 0 && anyValidDirective)

  private def anyValidDirective:Boolean = isDocumentEnd || currentChar == BomMark || isDirectivesEnd || isDirective

  def errorMapLine(spaces:Int, parent:Int): Boolean = {
    notDocument(parent) && consumeAndEmit(spaces, WhiteSpace) && {
      while (!isBreakComment(currentChar)) consume()
      emit(Error)
      breakComment()
    }
  }

  /**
    * [188]	ns-l-block-map-entry(n)	::=
    * [[mapExplicitEntry c-l-block-map-explicit-entry(n)]]
    * | [[mapImplicitEntry ns-l-block-map-implicit-entry(n)]]
    */
  @failfast def blockMapEntry(n: Int): Boolean = {
    val explicit = currentChar == '?'
    val entry    = explicit || lineContainsMapIndicator()
    if (!entry) false
    else
      matches {
        emit(BeginPair)
        (if (explicit) mapExplicitEntry(n) else mapImplicitEntry(n)) &&
        emit(EndPair)
      }
  }

  /**
    * Explicit map entries are denoted by the “?” mapping key indicator<p>
    * <blockquote><pre>
    * [189]	c-l-block-map-explicit-entry(n)	::=	c-l-block-map-explicit-key(n)
    * ( l-block-map-explicit-value(n) | [[emptyNode e-node]] )
    *
    * [190]	c-l-block-map-explicit-key(n)	::=	“?” [[blockIndented s-l+block-indented(n,block-out)]]
    *
    * </blockquote></pre>
    */
  @failfast def mapExplicitEntry(n: Int): Boolean = {
    val b = indicator('?')
    b && blockIndented(n, BlockOut) && matches {
      mapExplicitValue(n) || emptyNode(n)
    }
  }

  /**
    * [191]	l-block-map-explicit-value(n)	::=	[[indent s-indent(n)]] “:”
    * [\[blockIndented s-l+block-indented(n,block-out)]\]
    */
  def mapExplicitValue(n: Int): Boolean = matches {
    indent(n) && indicator(':') && blockIndented(n, BlockOut)
  }

  /**
    * Detect a Map implicit Entry
    *
    * <blockquote><pre>
    *
    * [192]	ns-l-block-map-implicit-entry(n) ::=
    * ( ns-s-block-map-implicit-key | [[emptyNode e-node]] )
    * [[mapImplicitValue c-l-block-map-implicit-value(n)]]
    *
    * [193]	ns-s-block-map-implicit-key	::=	  [[implicitJsonKey c-s-implicit-json-key(block-key)]]
    * |   [[implicitYamlKey ns-s-implicit-yaml-key(block-key)]]
    *
    *
    * </blockquote></pre>
    */
  def mapImplicitEntry(n: Int): Boolean = matches {
    (matches(implicitJsonKey(BlockKey)) || matches(implicitYamlKey(BlockKey)) || emptyNode(n)) &&
    mapImplicitValue(n)
  }

  /**
    * [194] c-l-block-map-implicit-value(n)	::=	“:”
    * ( [[blockNode s-l+block-node(n,block-out)]]
    * | ( [\[emptyNode e-node]\] [[multilineComment s-l-comments]] )
    * )
    */
  def mapImplicitValue(n: Int): Boolean = indicator(':') && {
    blockNode(n, BlockOut) ||
    emptyNode(n) && (matches(multilineComment()) || matches(error() && multilineComment()))
  }

  /**
    * [195]	ns-l-compact-mapping(n)	::=	[[blockMapEntry ns-l-block-map-entry(n)]]
    * ( [[indent s-indent(n)]] [[blockMapEntry ns-l-block-map-entry(n)]] )*
    */
  def compactMapping(n: Int): Boolean =
    emit(BeginMapping) && blockMapEntry(n) && zeroOrMore(indent(n) && blockMapEntry(n)) && emit(EndMapping)

  /**
    * [196] s-l+block-node(n,c)  ::=	s-l+block-in-block(n,c) | s-l+flow-in-block(n)
    * [197] s-l+flow-in-block(n) ::=	s-separate(n+1,flow-out) ns-flow-node(n+1,flow-out) s-l-comments
    */
  def blockNode(n: Int, ctx: YamlContext): Boolean =
    matches(blockInBlock(n, ctx)) || matches {
      val b1 = separate(n + 1, FlowOut) && emit(BeginNode)
      b1 && flowNode(n + 1, FlowOut) && emit(EndNode) && multilineComment()
    }

  /*
   *  [198]	s-l+block-in-block(n,c)	::=	s-l+block-scalar(n,c) | s-l+block-collection(n,c)
   */
  def blockInBlock(n: Int, ctx: YamlContext): Boolean =
    emit(BeginNode) && (blockScalar(n, ctx) || blockCollection(n, ctx)) && emit(EndNode)

  /**
    * A Block Scalar Node<p>
    * [199]	s-l+block-scalar(n,c)	::=	[[separate s-separate(n+1,c)]]
    * ([[nodeProperties c-ns-properties(n+1,c)]] [[separate s-separate(n+1,c)]])?
    * ( [[literalScalar c-l+literal(n)]] | [[foldedScalar c-l+folded(n)]] )
    */
  private def blockScalar(n: Int, ctx: YamlContext) = matches {
    separate(n + 1, ctx) && {
      if (nodeProperties(n + 1, ctx)) separate(n + 1, ctx)
      literalScalar(n) || foldedScalar(n)
    }
  }

  /**
    *
    * A nested Block Collection<p>
    * <blockquote><pre>
    * [200]	s-l+block-collection(n,c)	::=	( [[separate s-separate(n+1,c)]] c-ns-properties(n+1,c) )?
    * [[multilineComment s-l-comments]]
    * ( [[blockSequence l+block-sequence(seq-spaces(n,c))]]
    * | [[blockMapping]] l+block-mapping(n) )
    *
    * [201]	seq-spaces(n,c)	::=
    * c = block-out ⇒ n-1
    * c = block-in  ⇒ n
    *
    * </blockquote></pre>
    *
    */
  def blockCollection(n: Int, ctx: YamlContext): Boolean = {
    def bc() = multilineComment() && (blockSequence(if (ctx == BlockOut) n - 1 else n) || blockMapping(n))

    matches(separate(n + 1, ctx) && nodeProperties(n + 1, ctx) && bc()) || matches(bc())
  }

  /** A document may be preceded by a prefix specifying the character encoding, and optional comment lines.
    * Note that all documents in a stream must use the same character encoding.<p>
    *
    * [202]	l-document-prefix	::=	c-byte-order-mark? l-comment*
    */
  private def documentPrefix() =
    nonEof &&
      (currentChar == BomMark && consumeAndEmit(Bom) &&
        optional(breakComment() || commentText()) && zeroOrMore(lineComment()) ||
        oneOrMore(lineComment()))

  /**
    * The Directive End marker. Specifying false for the parameter will only test without emitting<p>
    *
    * [203]	c-directives-end	::=	“-” “-” “-”
    */
  @failfast private def isDirectivesEnd = currentChar == '-' && lookAhead(1) == '-' && lookAhead(2) == '-'

  /**
    * * The Document End marker. Specifying false for the parameter will only test without emitting<p>
    * [204]	c-document-end	::=	“.” “.” “.”
    */
  private def isDocumentEnd = currentChar == '.' && lookAhead(1) == '.' && lookAhead(2) == '.'

  /** [205]	l-document-suffix	::=	c-document-end s-l-comments */
  private def documentSuffix() = isDocumentEnd && matches(consumeAndEmit(3, DocumentEnd) && multilineComment())

  /**
    * A bare document does not begin with any directives or marker lines.<p>
    *
    * [207]	l-bare-document	::=	s-l+block-node(-1,block-in)
    */
  private def bareDocument() = blockNode(-1, BlockIn)

  /**
    * An explicit document begins with an explicit directives end marker line but no directives.
    * Since the existence of the document is indicated by this marker, the document itself may be completely empty.<p>
    *
    * [208]	l-explicit-document	::=
    * [[isDirectivesEnd c-directives-end]]
    * ( [[bareDocument l-bare-document]]
    * | ( [[emptyNode e-node]] [[multilineComment s-l-comments]] )
    * )
    */
  private def explicitDocument() = isDirectivesEnd && matches {
    consumeAndEmit(3, DirectivesEnd)
    matches(bareDocument()) || matches(emptyNode(0) && multilineComment())
  }

  /**
    * A directives document begins with some directives followed by an explicit directives end marker line.<p>
    *
    * [209]	l-directive-document	::=	l-directive+ l-explicit-document
    */
  private def directiveDocument() = isDirective && oneOrMore(directive()) && explicitDocument()

  private def isDirective:Boolean = currentChar == '%'

  /** [210]	l-any-document	::=	  l-directive-document | l-explicit-document | l-bare-document */
  private def anyDocument() = nonEof && matches {
    emit(BeginDocument) &&
    (directiveDocument() || explicitDocument() || bareDocument()) &&
    emit(EndDocument)
  }

  /**
    * A YAML stream consists of zero or more documents.
    * Subsequent documents require some sort of separation marker line.
    * If a document is not terminated by a document end marker line,
    * then the following document must begin with a directives end marker line.
    * The stream format is intentionally “sloppy” to better support common use cases, such as stream concatenation.
    *
    * <blockquote><pre>
    *
    * [211]	l-yaml-stream	::=
    * [[documentPrefix l-document-prefix]]*
    * [[anyDocument l-any-document]]?
    * (
    * l-document-suffix+ l-document-prefix* l-any-document?
    * | l-document-prefix* l-explicit-document?
    * )*
    * </blockquote></pre>
    */
  def yamlStream(): Unit = {
    while (documentPrefix()) {}
    anyDocument()
    var currentOffset = input.offset
    while (nonEof) {
      matches {
        oneOrMore(documentSuffix()) &&
        zeroOrMore(documentPrefix()) &&
        optional(anyDocument())
      } ||
      matches {
        zeroOrMore(documentPrefix())
        emit(BeginDocument) &&
        explicitDocument() &&
        emit(EndDocument)
      }
      if (input.offset == currentOffset) {
        consumeWhile(_ != EofChar)
        emit(Error)
      }
      currentOffset = input.offset
    }
    emit(EndStream)
  }

  /** Check if the line contains a map Indicator (":" plus space or end of text) */
  def lineContainsMapIndicator(): Boolean = {
    var i                    = 0
    var chr: Int             = 0
    val first = lookAhead(i)
    var dueClosing = obtainClosingChar(first)
    if(dueClosing.isDefined) i += 1
    do {
      chr = lookAhead(i)
      dueClosing match {
        case Some(closingChar) if chr == closingChar =>
          if (chr == '\'' && lookAhead(i + 1) == '\'') i = i + 1
          else if (chr != '"' || lookAhead(i - 1) != '\\') dueClosing = None
        case None if isMappingIndicator(chr, lookAhead(i + 1)) =>
          return true
        case _ =>
      }
      i += 1
    } while (!isBreakComment(chr))
    false
  }

  def obtainClosingChar(first: Int): Option[Int] = first match {
    case '"' | '\'' => Some(first)
    case '{' => Some('}'.toInt)
    case '[' =>  Some(']'.toInt)
    case _ => None
  }

  /** Emit an error and Consume until end of line or file */
  def error(): Boolean =
    if (isBreakComment(currentChar)) true
    else {
      do consume() while (!isBreakComment(currentChar))
      emit(Error)
    }

  private def nonEndOfDocument = { // Todo optimize
    !beginOfLine || !isDocumentEnd && !isDirectivesEnd
  }

  private def restoreState(s: (Int, Position, Mark)): Unit = {
    tokenQueue.reduceTo(s._1)
    mark = s._2
    input.reset(s._3)
  }

  private def saveState: (Int, Position, Mark) = (tokenQueue.size, mark, input.createMark())

  private def matches(p: => Boolean): Boolean = {
    val s      = saveState
    val result = p
    if (!result) restoreState(s)
    result
  }

  private def zeroOrMore(p: => Boolean): Boolean = {
    var s = saveState
    while (nonEof && p) s = saveState
    restoreState(s)
    true
  }

  private def oneOrMore(p: => Boolean): Boolean = {
    var s      = saveState
    val result = p
    if (result) {
      do s = saveState while (nonEof && p)
    }
    restoreState(s)
    result
  }

}

object YamlLexer {

  def isText(c: Int): Boolean = c != '\n' && c != '\r' && c != EofChar

  def apply(): YamlLexer                  = new YamlLexer(CharSequenceLexerInput())
  def apply(input: LexerInput): YamlLexer = new YamlLexer(input)
  def apply(cs: CharSequence): YamlLexer  = new YamlLexer(CharSequenceLexerInput(cs))
  def apply(cs: CharSequence, sourceName: String): YamlLexer =
    new YamlLexer(CharSequenceLexerInput(cs, sourceName = sourceName))

  @deprecated("Use Position argument", "") def apply(cs: CharSequence, positionOffset: (Int, Int)): YamlLexer =
    YamlLexer(cs, Position(positionOffset._1, positionOffset._2))

  @deprecated("Use Position argument", "") def apply(cs: CharSequence,
                                                     sourceName: String,
                                                     positionOffset: (Int, Int)): YamlLexer =
    YamlLexer(cs, sourceName, Position(positionOffset._1, positionOffset._2))

  def apply(cs: CharSequence, positionOffset: Position): YamlLexer =
    new YamlLexer(CharSequenceLexerInput(cs), positionOffset)

  def apply(cs: CharSequence, sourceName: String, positionOffset: Position): YamlLexer =
    new YamlLexer(CharSequenceLexerInput(cs, sourceName = sourceName), positionOffset)
}

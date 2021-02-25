package org.yaml.render

import org.mulesoft.common.core.Strings
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.mulesoft.lexer.AstToken
import org.yaml.lexer.YamlToken
import org.yaml.model.{YDocument, _}

import java.io.StringWriter

/**
  * Yaml Render
  */
class YamlRender[W: Output](val writer: W, val expandReferences: Boolean, initialIndentation:Int = 0, protected val options: YamlRenderOptions = YamlRenderOptions()) {
  protected val buffer = new StringBuilder

  protected var indentation: Int = initialIndentation - options.indentationSize
  protected def indent(): Unit = indentation += options.indentationSize
  protected def dedent(): Unit = indentation -= options.indentationSize
  protected def renderIndent(): this.type = {
    print(" " * indentation)
    this
  }
  private var hasDirectives = false
  private var endDocument   = false

  def renderParts(parts: Seq[YPart]) {
    parts.foreach(render(_, None))
    flushBuffer()
  }

  private def flushBuffer(): Unit = if (buffer.nonEmpty) {
    writer.append(buffer.toString)
    buffer.clear()
  }
  protected def print(value: String): this.type = {
    buffer.append(value)
    this
  }
  private def println() =
    if (buffer.isEmpty) this
    else {
      val last = buffer.length - 1
      if (last >= 0 && buffer(last).isWhitespace) buffer.setLength(last)
      buffer.append('\n')
      flushBuffer()
      this
    }

  protected def render(part: YPart, yType: Option[YType] = None): this.type = {
    checkEndDocument(part)
    part match {
      case c: YComment     => renderComment(c.metaText, c.tokens)
      case nc: YNonContent => nc.tokens foreach renderToken
      case d: YDocument    => renderDocument(d.children)
      case d: YDirective   => renderDirective(d)
      case s: YSequence    => renderSeq(s)
      case m: YMap         => renderMap(m)
      case e: YMapEntry    => doRenderParts(e.children)
      case s: YScalar      => renderScalar(s, yType.contains(YType.Str))
      case t: YTag         => renderTag(t)
      case a: YAnchor      => renderAnchor(a)
      case n: YNode        => renderNode(n)
    }
    this
  }

  private def checkEndDocument(part: YPart) = {
    if (endDocument) {
      endDocument = false
      print("...\n")
      part match {
        case doc: YDocument if doc.tagType == YType.Null => print("---\n")
        case _                                           =>
      }
    }
  }

  private def renderTag(t: YTag) = if (!renderTokens(t.tokens) && !t.synthesized) print(t.toString + " ")

  private def renderNode(n: YNode): Unit = if (expandReferences && n.isInstanceOf[YNode.Ref] || !renderParts(n)) {
    if (hasDirectives) {
      print("---\n")
      hasDirectives = false
    }
    n match {
      case a: YNode.Alias =>
        if (expandReferences) render(a.target) else print(a.toString)
      case r: YNode.MutRef if expandReferences && r.target.isDefined =>
        render(r.target.get)
      case _ =>
        doRenderParts(n.children, if (n.tag == YType.Str.tag) Some(YType.Str) else None)
    }
  }

  private def renderAnchor(anchor: YAnchor) = if (!renderTokens(anchor.tokens)) print(anchor + " ")
  private def renderDirective(d: YDirective): Unit = {
    if (!renderParts(d)) print(d.toString).println()
    hasDirectives = true
  }

  private def renderDocument(parts: IndexedSeq[YPart]): Unit = {
    doRenderParts(parts)
    println()
    endDocument = true
  }

  protected def renderMap(map: YMap): Unit = if (!renderParts(map)) {
    if (map.isEmpty) print("{}")
    else if (map.isInFlow && !options.applyFormatting) renderAsFlow(map)
    else {
      indent()
      for (e <- map.entries) println().renderIndent().renderMapEntry(e)
      dedent()
    }
  }

  protected def renderMapEntry(e: YMapEntry): Unit = {
    // The key
    val key = e.key
    key.value match {
      case s: YScalar =>
        renderTag(key.tag)
        for (r <- key.anchor) render(r)
        if (s.text contains "\n")
          print('"' + s.text.encode + '"')
        else {
          val mustBeString = key.tagType == YType.Str && key.tag.synthesized
          renderScalar(s, mustBeString)
        }
        print(": ")
      case _ =>
        print("?").render(key).println().renderIndent().print(": ")
    }

    // Capture comments before and after the value
    val value = e.value
    val (before, tail) = e.children
      .dropWhile(!_.eq(key))
      .tail
      .dropWhile(c =>
        c.isInstanceOf[YNonContent] && c.asInstanceOf[YNonContent].tokens.headOption.exists(t => t.text == ":"))
      .span(!_.eq(value))
    val after = tail.tail

    // Render Before comments
    indent()
    for (c <- before) render(c).renderIndent()
    dedent()

    // Render the value (special case Null as Empty)
    if (value.tagType != YType.Null || value.toString.nonEmpty) render(value)

    // Render after comments
    if (after.nonEmpty) {
      render(after.head)
      indent()
      for (c <- after.tail) renderIndent().render(c)
      dedent()
    }
  }

  private def renderComment(text: String, tks: IndexedSeq[AstToken]) = {
    if (options.applyFormatting) printComment(text)
    else if (!renderTokens(tks)) {
      printComment(text)
      println()
    }
  }

  private def printComment(text: String) = {
    if (buffer.nonEmpty && !buffer.last.isWhitespace) print(" ")
    print("#" + (if (!text.startsWith(" ") && options.applyFormatting) " " else "") + text)
  }

  private def renderScalar(scalar: YScalar, mustBeString: Boolean = false): Unit =
    if (!renderParts(scalar)) {
      val str = ScalarRender.renderScalar(
          text = scalar.text,
          mustBeString = mustBeString,
          mark = scalar.mark,
          indentation = indentation,
          firstLineComment = scalar.children.collectFirst { case c: YComment => " #" + c.metaText }.getOrElse("")
      )
      print(str.toString)
    }

  protected def renderSeq(seq: YSequence): Unit = if (!renderParts(seq)) {
    if (seq.isEmpty) print("[]")
    else if (seq.isInFlow && !options.applyFormatting) renderAsFlow(seq)
    else {
      indent()
      for (e <- seq.children) {
        e match {
          case n: YNode    => println().renderIndent().print("- ").render(n)
          case c: YComment => render(c)
          case n: YNonContent if options.applyFormatting =>
            // if we apply formatting we should still preserve the linebreaks on the sequence
            renderTokens(n.tokens.filter(p => p.tokenType == YamlToken.LineBreak))
          case _           =>
        }
      }
      dedent()
    }
  }

  private def renderTokens(tks: IndexedSeq[AstToken]): Boolean = {
    val hasTokens = tks.nonEmpty
    if (hasTokens) tks foreach renderToken
    hasTokens
  }
  private def renderToken(t: AstToken): Unit = print(t.text)

  private def renderParts(parts: YPart): Boolean = {
    val nodes     = parts.children
    val hasTokens = nodes.nonEmpty && nodes.head.isInstanceOf[YNonContent] && !options.applyFormatting
    if (hasTokens) doRenderParts(nodes)
    hasTokens
  }
  private def doRenderParts(children: IndexedSeq[YPart], yType: Option[YType] = None): Unit = children foreach {
    render(_, yType)
  }

  private def renderAsFlow(ypart: YPart): Unit = {
    indent()
    buildFlowRenderer().render(ypart)
    dedent()
  }

  private def buildFlowRenderer(): YamlFlowRender[W] = {
    new YamlFlowRender[W](writer, expandReferences, indentation, options, buffer)
  }
}

object YamlRender {

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int, options: YamlRenderOptions): Unit =
    new YamlRender(writer, expandReferences, indentation, options).renderParts(parts)

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int = 0): Unit =
  render(writer, parts, expandReferences, indentation, YamlRenderOptions())

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart]): Unit = render(writer, parts, expandReferences = false)

  /** Render a YamlPart to a Writer */
  def render[W: Output](writer: W, part: YPart): Unit = render(part, expandReferences = false)

  /** Render a YamlPart to a Writer */
  def render[W: Output](writer: W, part: YPart, expandReferences: Boolean): Unit =
    render(writer, Seq(part), expandReferences)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart]): String = render(parts, expandReferences = false)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], options: YamlRenderOptions): String = render(parts,expandReferences = false, options, 0)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean, options: YamlRenderOptions, initialIndentation: Int): String = {
    val s = new StringWriter
    render(s, parts, expandReferences, initialIndentation, options)
    s.toString
  }

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean): String = {
    render(parts, expandReferences, YamlRenderOptions(), 0)
  }

  /** Render a YamlPart as an String */
  def render(part: YPart): String = render(part, expandReferences = false)

  /** Render a YamlPart as an String */
  def render(part: YPart, expandReferences: Boolean): String = {
    val s = new StringWriter
    render(s, part, expandReferences)
    s.toString
  }

  /** Render a YamlParts as an String starting at a given indentation*/
  def render(parts: YPart, indentation:Int, options: YamlRenderOptions): String = {
    val s = new StringWriter
    render(s, Seq(parts),expandReferences = false, indentation = indentation, options)
    s.toString
  }

  /** Render a YamlParts as an String starting at a given indentation*/
  def render(parts: YPart, indentation:Int): String = {
    val s = new StringWriter
    render(s, Seq(parts),expandReferences = false, indentation = indentation)
    s.toString
  }
}

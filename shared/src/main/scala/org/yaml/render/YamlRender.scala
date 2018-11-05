package org.yaml.render

import java.io.StringWriter

import org.mulesoft.common.core.Strings
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.mulesoft.lexer.AstToken
import org.yaml.model._

/**
  * Yaml Render
  */
class YamlRender[W: Output](val writer: W, val expandReferences: Boolean) {
  private val buffer = new StringBuilder

  private var indentation    = -2
  private def indent(): Unit = indentation += 2
  private def dedent(): Unit = indentation -= 2
  private def renderIndent(): this.type = {
    for (_ <- 0 until indentation) buffer append ' '
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
  private def print(value: String) = {
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

  private def render(part: YPart, yType: Option[YType] = None): this.type = {
    checkEndDocument(part)
    part match {
      case YComment(text, _, tokens) => renderComment(text, tokens)
      case YNonContent(_, tokens, _) => tokens foreach renderToken
      case YDocument(parts, _)       => renderDocument(parts)
      case d: YDirective             => renderDirective(d)
      case s: YSequence              => renderSeq(s)
      case m: YMap                   => renderMap(m)
      case e: YMapEntry              => doRenderParts(e.children)
      case s: YScalar                => renderScalar(s, yType.contains(YType.Str))
      case t: YTag                   => renderTag(t)
      case a: YAnchor                => renderAnchor(a)
      case n: YNode                  => renderNode(n)
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

  private def renderMap(map: YMap): Unit = if (!renderParts(map)) {
    if (map.isEmpty) print("{}")
    else {
      indent()
      for (e <- map.entries) println().renderIndent().renderMapEntry(e)
      dedent()
    }
  }

  private def renderMapEntry(e: YMapEntry): Unit = {
    // The key
    val key = e.key
    key.value match {
      case s: YScalar =>
        renderTag(key.tag)
        for (r <- key.anchor) render(r)
        if (s.text contains "\n")
          print('"' + s.text.encode + '"')
        else
          renderScalar(s)
        print(": ")
      case _ =>
        print("?").render(key).println().renderIndent().print(": ")
    }

    // Capture comments before and after the value
    val value          = e.value
    val (before, tail) = e.children.dropWhile(!_.eq(key)).tail.span(!_.eq(value))
    val after          = tail.tail

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

  private def renderComment(text: String, tks: IndexedSeq[AstToken]) = if (!renderTokens(tks)) {
    if (buffer.nonEmpty && !buffer.last.isWhitespace) print(" ")
    print("#" + text).println()
  }

  private def renderScalar(scalar: YScalar, mustBeString: Boolean = false): Unit =
    if (!renderParts(scalar)) {
      val str = ScalarRender.renderScalar(
          text = scalar.text,
          mustBeString = mustBeString,
          plain = scalar.plain,
          indentation = indentation,
          firstLineComment = scalar.children.collectFirst { case YComment(txt, _, _) => s" #$txt" }.getOrElse("")
      )
      print(str.toString)
    }

  private def renderSeq(seq: YSequence): Unit = if (!renderParts(seq)) {
    if (seq.isEmpty) print("[]")
    else {
      indent()
      for (e <- seq.children) {
        e match {
          case n: YNode    => println().renderIndent().print("- ").render(n)
          case c: YComment => render(c)
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
    val hasTokens = nodes.nonEmpty && nodes.head.isInstanceOf[YNonContent]
    if (hasTokens) doRenderParts(nodes)
    hasTokens
  }

  private def doRenderParts(children: IndexedSeq[YPart], yType: Option[YType] = None): Unit = children foreach {
    render(_, yType)
  }

}

object YamlRender {

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart]): Unit = render(writer, parts, expandReferences = false)

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean): Unit =
    new YamlRender(writer, expandReferences).renderParts(parts)

  /** Render a YamlPart to a Writer */
  def render[W: Output](writer: W, part: YPart): Unit = render(part, expandReferences = false)

  /** Render a YamlPart to a Writer */
  def render[W: Output](writer: W, part: YPart, expandReferences: Boolean): Unit =
    render(writer, Seq(part), expandReferences)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart]): String = render(parts, expandReferences = false)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean): String = {
    val s = new StringWriter
    render(s, parts, expandReferences)
    s.toString
  }

  /** Render a YamlPart as an String */
  def render(part: YPart): String = render(part, expandReferences = false)

  /** Render a YamlPart as an String */
  def render(part: YPart, expandReferences: Boolean): String = {
    val s = new StringWriter
    render(s, part, expandReferences)
    s.toString
  }
}

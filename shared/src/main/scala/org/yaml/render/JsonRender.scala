package org.yaml.render

import org.mulesoft.common.core._
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.mulesoft.lexer.AstToken
import org.yaml.lexer.YamlToken
import org.yaml.model.YType._
import org.yaml.model._

import java.io.StringWriter

/**
  * Json Render
  */
class JsonRender[W: Output] private (private val writer: W,
                                     initialIndentation: Int = 0,
                                     options: JsonRenderOptions = JsonRenderOptions()) {
  override def toString: String = writer.toString

  private var indentation    = initialIndentation
  private def indent(): Unit = indentation += options.indentationSize
  private def dedent(): Unit = indentation -= options.indentationSize
  private def renderIndent(): this.type = {
    if (indentation > 0) {
      if (options.preferSpaces) writer.append(" " * indentation)
      else writer.append("\t" * (indentation / options.indentationSize))
    }
    this
  }
  private def render(node: YNode): this.type = {
    node.value match {
      case m: YMap      => renderMap(m)
      case s: YSequence => renderSeq(s)
      case s: YScalar   => renderScalar(node.tagType, s)
    }
    this
  }

  def renderDocument(document: YDocument): JsonRender[W] = {
    if (document.node == YNode.Null) render("{}")
    else {
      document.children.foreach(renderIndent().render(_).render("\n"))
    }
    this
  }

  def renderLinebreaks(nc: YNonContent, skips: Int = 0): Int = {
    val linebreaks   = nc.tokens.filter(_.tokenType == YamlToken.LineBreak)
    val shouldRender = linebreaks.drop(skips)
    renderTokens(shouldRender)
    linebreaks.length - shouldRender.length // Returns the amount of linebreaks skipped
  }

  private def renderTokens(tks: IndexedSeq[AstToken]): Boolean = {
    val hasTokens = tks.nonEmpty && options.applyFormatting
    if (hasTokens) tks foreach renderToken
    hasTokens
  }

  private def renderToken(t: AstToken): Unit = render(t.text)

  private def render(yPart: YPart): JsonRender[W] = {
    yPart match {
      case node: YNode         => render(node)
      case entry: YMapEntry    => renderEntry(entry)
      case map: YMap           => renderMap(map)
      case document: YDocument => renderDocument(document)
      case _: YNonContent      => this // Consume non-content
      case other               => render(other.toString)
    }
  }

  private def renderSeq(seq: YSequence) =
    if (seq.isEmpty) render("[]")
    else {
      render("[\n")
      indent()
      val total          = seq.nodes.size
      val count          = seq.children.size
      var c              = 0
      var i              = 0
      var linebreakSkips = 1

      while (i < count) {
        seq.children(i) match {
          case node: YNode =>
            renderIndent().render(node).render(if (c < total - 1) ",\n" else "\n")
            c += 1
            linebreakSkips += 1
          case nc: YNonContent if options.applyFormatting => linebreakSkips -= renderLinebreaks(nc, linebreakSkips)
          case _                                          =>
        }
        i += 1
      }
      dedent()
      renderIndent().render("]")
    }

  private def renderMap(map: YMap) =
    if (map.isEmpty) render("{}")
    else {
      render("{\n")
      indent()
      val total          = map.entries.size
      val count          = map.children.size
      var c              = 0
      var i              = 0
      var linebreakSkips = 1

      while (i < count) {
        map.children(i) match {
          case entry: YMapEntry =>
            renderIndent().renderEntry(entry).render(if (c < total - 1) ",\n" else "\n")
            c += 1
            linebreakSkips += 1
          case nc: YNonContent if options.applyFormatting => linebreakSkips -= renderLinebreaks(nc, linebreakSkips)
        }
        i += 1
      }
      dedent()
      renderIndent().render("}")
    }

  private def renderEntry(entry: YMapEntry) = {
    render(entry.key).render(": ").render(entry.value)
  }

  private def renderScalar(t: YType, scalar: YScalar): Unit =
    render(t match {
      case Int | Bool => scalar.value.toString
      case Float =>
        val s = scalar.value.toString
        if (s.indexOf('.') == -1 && !s.contains('e') && !s.contains('E')) s + ".0" else s // Bug in scala-js toString
      case Null => ""
      case _ =>
        scalar.value match {
          case s: String => '"' + s.encode(encodeNonAscii = options.encodesNonAscii) + '"'
          case _         => '"' + scalar.text + '"'
        }
    })

  private def render(value: String) = {
    writer.append(value)
    this
  }
}

object JsonRender {

  /** Render a Seq of Parts to an Output */
  def render[W: Output](doc: YDocument,
                        writer: W,
                        indentation: Int = 0,
                        options: JsonRenderOptions = JsonRenderOptions()): Unit = {
    try {
      val builder = new JsonRender(writer, indentation, options)
      builder.render(doc.node).render("\n")
    } finally {
      writer.flush
    }
  }

  def render(doc: YDocument, indentation: Int, options: JsonRenderOptions): String = {
    val s = new StringWriter()
    render(doc, s, indentation, options)
    s.toString
  }

  /** Render a Seq of Parts as a String */
  def render(doc: YDocument, indentation: Int): String = render(doc, indentation, JsonRenderOptions())

  def render(doc: YDocument): String = render(doc, 0)

  def render(part: YPart, indentation: Int, options: JsonRenderOptions): String = {
    val s       = new StringWriter()
    val builder = new JsonRender(s, indentation, options)
    builder.render(part)
    s.toString
  }

  def render(part: YPart, indentation: Int): String = render(part, indentation, JsonRenderOptions())

}

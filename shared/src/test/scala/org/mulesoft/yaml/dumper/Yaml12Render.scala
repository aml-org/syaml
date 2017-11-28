package org.mulesoft.yaml.dumper

import org.mulesoft.common.core._
import org.yaml.model._

/**
  * A Render to Yam 1.2 expanded Format
  */
class Yaml12Render(val parts: Seq[YPart], output: StringBuilder) {
  def this(doc: YDocument, output: StringBuilder) = this(List(doc), output)

  var indentation = 2
  private var indent = ""

  def dump(): Unit = {
    parts foreach {
      case doc: YDocument =>
          output append "%YAML 1.2\n"
          output append  "---\n"
          dump(doc.node)
          output append '\n'
      case _ =>
    }
  }

  private def dump(node: YNode, mark: String = ""): Unit = {
    output append indent + mark

    node.ref match {
        case Some(a:YAlias) =>
            output append a
            return
        case Some(a:YAnchor) =>
            printTag(node)
            output append a + " "
        case _ =>
            printTag(node)
    }

    node.value match {
      case scalar: YScalar =>
          output append '"' + scalar.text.encode + '"'
      case seq: YSequence  => printEntries[YNode]("[", seq.nodes, "]", dump(_))
      case map: YMap =>
        printEntries[YMapEntry]("{", map.entries, "}", e => {
          dump(e.key, "? ")
          output append '\n'
          dump(e.value, ": ")
        })
    }
  }

    private def printTag(node: YNode) = {
        var tag = node.tag
        if (tag.text == "!") tag = tag.tagType.tag
        output append tag + " "
    }

    private def printEntries[T](prefix: String, seq: IndexedSeq[T], suffix: String, pf: T => Unit) = {
    output append prefix append '\n'
    indent += " " * indentation
    var first = true
    for (e <- seq) {
      if (!first) output append ",\n" else first = false
      pf(e)
    }
    output append '\n'
    indent = indent.substring(indentation)
    output append indent + suffix
  }

}
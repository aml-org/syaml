package org.yaml.render

import org.mulesoft.common.io.Output
import org.yaml.model.{YMap, YSequence}

class YamlFlowRender[W: Output] (override val writer: W,
                                override val expandReferences: Boolean,
                                initialIndentation:Int = 0,
                                options: YamlRenderOptions = YamlRenderOptions(),
                                override protected val buffer: StringBuilder = new StringBuilder
                               ) extends YamlRender(writer, expandReferences, initialIndentation + options.indentationSize, options) {

  override protected def renderMap(map: YMap): Unit = {
    if (map.isEmpty) print("{}")
    else {
      val total          = map.entries.size
      var c              = 0
      print("{\n")
      while (c < total) {
        indent()
        val entry = map.entries(c)
        renderIndent().renderMapEntry(entry)
        print(if (c < total - 1) ",\n" else "\n")
        c += 1
        dedent()
      }
      renderIndent().print("}")
    }
  }

  override protected def renderSeq(seq: YSequence): Unit =
    if (seq.isEmpty) print("[]")
    else {
      print("[\n")
      indent()
      val total          = seq.nodes.size
      var c              = 0
      while (c < total) {
        renderIndent().render(seq.nodes(c)).print(if (c < total - 1) ",\n" else "\n")
        c += 1
      }
      dedent()
      renderIndent().print("]")
    }
}

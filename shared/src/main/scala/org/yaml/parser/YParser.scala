package org.yaml.parser

import org.yaml.model.{YDocument, YNode, YPart}

trait YParser {
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart]
  def documents(): IndexedSeq[YDocument]
  def document(keepTokens:Boolean = false): YDocument = documents().headOption.getOrElse(YNode.Null)
}

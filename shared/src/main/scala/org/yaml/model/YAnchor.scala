package org.yaml.model

import org.mulesoft.lexer.{InputRange, AstToken}

/**
  * A YReference is either an anchor or an alias
  */
class YAnchor private (val name: String, range: InputRange, ts: IndexedSeq[AstToken]) extends YTokens(range, ts) {
  override def toString: String = "&" + name
}

object YAnchor {
  def apply(name: String, range: InputRange, ts: IndexedSeq[AstToken]): YAnchor = new YAnchor(name, range, ts)
  def apply(name: String): YAnchor = new YAnchor(name, InputRange.Zero, IndexedSeq.empty)

}

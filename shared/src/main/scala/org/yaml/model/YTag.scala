package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.mulesoft.lexer.InputRange.Zero
import org.yaml.lexer.YeastToken

/**
  * A Yaml Tag
  */
case class YTag(text: String,
                tagType: YType,
                override val range: InputRange = Zero,
                override val tokens: IndexedSeq[YeastToken] = IndexedSeq.empty)
    extends YTokens(range, tokens) {

  def synthesized: Boolean      = tagType.synthesized && tagType.tag == this
  override def toString: String = text
}

object YTag {
  def apply(tag: String, range: InputRange, ts: IndexedSeq[YeastToken]): YTag = YTag(tag, YType(tag), range, ts)
  def apply(tag: String): YTag                                                = YTag(tag, YType(tag))
}

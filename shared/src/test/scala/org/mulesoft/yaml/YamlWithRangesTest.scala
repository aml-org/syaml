package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YMap
import org.yaml.parser.YamlParser

class YamlWithRangesTest extends FunSuite with Matchers {

  test("assert range of empty two lines value"){
    val text = "a:\n  "
    val documents = YamlParser(text).documents()
    assert(documents.length==1)
    val value = documents.head.node.as[YMap].entries.head.value
    val scalar = value.asScalar
    assert(scalar.isDefined)
    val range = scalar.get.range
    assert(range.lineFrom==1)
    assert(range.lineTo==2)
    assert(range.columnTo==2)
  }
}

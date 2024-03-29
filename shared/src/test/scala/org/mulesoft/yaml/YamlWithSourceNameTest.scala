package org.mulesoft.yaml

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.yaml.model.YPart
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
trait YamlWithSourceNameTest extends AnyFunSuite with Matchers {

  private val rootMap =
    """
      |map:
      | entry1: a
      | entry2: b
      | entry3:
      |   map:
      |     entry4: c
      |     entry5:
      |       - a
      |       - b
      |       - entry6: d
      |       - map:
      |           entry7: e
    """.stripMargin

  private val rootSeq =
    """
      |- map:
      |   entry1: a
      |   entry2:
      |     map:
      |       entry3: b
      |       entry4:
      |         - a
      |         - b
      |   entry3:
      |     - a
      |     - b
      |- b
    """.stripMargin


  private val rootScalar =
    """scalar""".stripMargin
  private val sourceName = "sourcename.yaml"

  test("assert source name root map"){
    val document = YamlParser(rootMap,sourceName).document()
    assertNameInChild(document)
  }

  test("assert source name root seq"){
    val document = YamlParser(rootSeq,sourceName).document()
    assertNameInChild(document)
  }

  test("assert source name root scalar"){
    val document = YamlParser(rootScalar,sourceName).document()
    assertNameInChild(document)
  }

  test("assert empty source name root map"){
    val document = YamlParser(rootMap,"").document()
    assertEmptySourceName(document)
  }


  private def assertNameInChild(part:YPart):Assertion = {
    part.sourceName should be(sourceName)
    part.children.foreach { assertNameInChild }
    succeed
  }

  private def assertEmptySourceName(part:YPart):Assertion = {
    part.sourceName should be("")
    part.children.foreach { assertEmptySourceName }
    succeed
  }
}
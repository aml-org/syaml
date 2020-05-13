package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.lexer.{InputRange, SourceLocation}
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, SyamlException, YMap, YPart}
import org.yaml.parser.YamlParser

import scala.collection.mutable

/**
  * Test handling errors
  */
trait ParseInvalidYamlsTest extends FunSuite {

  private val yamlDir = Fs syncFile "shared/src/test/data/parser/invalid"

  test("Parse duplicate key") {
    val yamlFile = yamlDir / "duplicate-key.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(2) == 0)
    val normal = handler.errors.head
    assert(normal.error.getMessage.equals("Duplicate key : 'normal'"))
    assert(normal.inputRange.equals(InputRange(2, 0, 2, 6)))
    val quotation = handler.errors(1)
    assert(quotation.error.getMessage.equals("Duplicate key : 'withQuotation'"))
    assert(quotation.inputRange.equals(InputRange(4, 0, 4, 15)))
  }

  test("Parse invalid entry value as scalar and map") {
    val yamlFile = yamlDir / "invalid-entry-value.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(2) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'name'"))
    assert(handler.errors.last.error.getMessage.equals("Syntax error in the following text: '  get:\n  post:'"))
  }

  test("Parse invalid alias referencing undefined anchor") {
    val yamlFile = yamlDir / "invalid-alias.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Undefined anchor : 'other'"))
  }

  test("Single Unclosed key at root") {
    val handler = TestErrorHandler()

    val text =
      """key1
        |otherkey: value
        |""".stripMargin
    val doc = YamlParser(text)(handler).document()
    val map = doc.node.as[YMap]
    assert(map.entries.length == 1)
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'key1\n'"))
  }

  test("Unclosed key at root in middle") {
    val handler = TestErrorHandler()

    val text =
      """otherkey: value
        |invalid
        |recover: value
        |""".stripMargin
    val docs = YamlParser(text)(handler).documents()
    assert(docs.head.node.as[YMap].entries.length == 2)
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'invalid\n'"))
  }

  test("Two Unclosed key at root") {
    val handler = TestErrorHandler()

    val text =
      """key1
        |key2
        |otherkey: value
        |""".stripMargin
    val doc = YamlParser(text)(handler).document()
    val map = doc.node.as[YMap]
    assert(map.entries.length == 1)
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'key1\nkey2\n'"))
  }

  test("Unclosed key and recover") {
    val handler = TestErrorHandler()

    val text =
      """key1: valie
        |key2:
        |   otherLevel:
        |     invalid
        |     valid: value
        |   brother
        |key3:
        |""".stripMargin
    val doc = YamlParser(text)(handler).document()
    val map  = doc.node.as[YMap]
    assert(map.entries.length == 3)
    val secondMap = map.entries(1).value.as[YMap]
    assert(secondMap.entries.length == 1)
    assert(secondMap.entries.head.value.as[YMap].entries.length == 1)

    assert(handler.errors.lengthCompare(2) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: '     invalid\n'"))
    assert(handler.errors.last.error.getMessage.equals("Syntax error in the following text: 'brother\n'"))
  }

  test("Valid map with unclosed key in middle and recover") {
    val handler = TestErrorHandler()

    val text =
      """key:
        |  valid1: value
        |  invalid
        |  valid2: value""".stripMargin
    val doc = YamlParser(text)(handler).document()
    val map = doc.node.as[YMap]
    val secondMap = map.entries.head.value.as[YMap]
    assert(secondMap.entries.length == 2)
  }

  test("Invalid first key in non root map recover") {
    val handler = TestErrorHandler()

    val text =
      """key:
        |  Unclosed key at root in middle
        |  valid1: value""".stripMargin
    val doc = YamlParser(text)(handler).document()
    val map = doc.node.as[YMap]
    val secondMap = map.entries.head.value.as[YMap]
    assert(secondMap.entries.length == 1)
  }

  test("Sequence recovery in middle") {
    val handler = TestErrorHandler()

    val text =
      """key:
        |  - seq1
        |  error
        |  - seq2""".stripMargin
    val docs = YamlParser(text)(handler).documents()
    assert(docs.length == 1)
    val map = docs.head.node.as[YMap]
    assert(map.entries.length == 1)
    val seq: Seq[String] = map.entries.head.value.as[Seq[String]]
    assert(seq.length ==  2)
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'error\n'"))
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(loc: SourceLocation, e: SyamlException): Unit = errors += ErrorContainer(e, loc.inputRange)
  }

}

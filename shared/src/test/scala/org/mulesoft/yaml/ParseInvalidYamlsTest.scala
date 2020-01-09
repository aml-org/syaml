package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.lexer.{InputRange, SourceLocation}
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, SyamlException, YMap}
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

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Duplicate key : 'first'"))
    assert(handler.errors.head.inputRange.equals(InputRange(3, 0, 3, 5)))
  }

  test("Parse invalid entry value as scalar and map") {
    val yamlFile = yamlDir / "invalid-entry-value.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(3) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: ' name'"))
    assert(handler.errors(1).error.getMessage.equals("Syntax error in the following text: 'get:'"))
    assert(handler.errors.last.error.getMessage.equals("Syntax error in the following text: 'post:'"))
  }

  test("Parse invalid alias referencing undefined anchor") {
    val yamlFile = yamlDir / "invalid-alias.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Undefined anchor : 'other'"))
  }

  test("Unclosed key at root") {
    val handler = TestErrorHandler()

    val text =
      """key1
        |otherkey: value
        |""".stripMargin
    val docs = YamlParser(text)(handler).documents()
    assert(docs.head.node.as[YMap].entries.length == 1)
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'key1'"))
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
    val docs = YamlParser(text)(handler).documents()
    assert(docs.length == 1)
    val map = docs.head.node.as[YMap]
    assert(map.entries.length == 3)
    val secondMap = map.entries(1).value.as[YMap]
    assert(secondMap.entries.length == 1)
    assert(secondMap.entries.head.value.as[YMap].entries.length == 1)

    assert(handler.errors.lengthCompare(2) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: 'invalid'"))
    assert(handler.errors.last.error.getMessage.equals("Syntax error in the following text: 'brother'"))
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(loc: SourceLocation, e: SyamlException): Unit = errors += ErrorContainer(e, loc.inputRange)
  }

}

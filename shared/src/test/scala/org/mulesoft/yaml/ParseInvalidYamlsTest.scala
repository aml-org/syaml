package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.lexer.{InputRange, SourceLocation}
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, SyamlException, YPart}
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
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: ' name'"))
    assert(handler.errors.last.error.getMessage.equals("Syntax error in the following text: '  get:\n  post:'"))
  }

  test("Parse invalid alias referencing undefined anchor") {
    val yamlFile = yamlDir / "invalid-alias.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Undefined anchor : 'other'"))
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(loc: SourceLocation, e: SyamlException): Unit = errors += ErrorContainer(e, loc.inputRange)
  }

}

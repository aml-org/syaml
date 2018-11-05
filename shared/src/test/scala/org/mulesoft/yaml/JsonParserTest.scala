package org.mulesoft.yaml

import org.mulesoft.common.io.{Fs, SyncFile}
import org.mulesoft.lexer.InputRange
import org.scalatest.FunSuite
import org.yaml.model._
import org.yaml.parser.JsonParser

import scala.collection.mutable

/**
  * Test handling errors
  */
trait JsonParserTest extends FunSuite {

  private val jsonDir = Fs syncFile "shared/src/test/data/invalid-json"

  test("Parse unquoted key") {
    val handler = getErrorsFor(jsonDir / "unquoted-key.json")

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.inputRange.equals(InputRange(3, 2, 3, 5)))

  }

  ignore("Parse unquoted value in seq") {
    val handler = getErrorsFor(jsonDir / "unquoted-value-seq.json.ignore")

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Error node 'invalid'"))
    assert(handler.errors.head.inputRange.equals(InputRange(3, 10, 3, 17)))
  }

  test("Parse scalar in map entry in map") {
    val handler = getErrorsFor(jsonDir / "scalar-in-map-entry.json")

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: '#an invalid json comment only to check the raw render'"))
    assert(handler.errors.head.inputRange.equals(InputRange(5, 4, 5, 57)))
  }

  test("Parse unclosed map") {
    val handler = getErrorsFor(jsonDir / "unclosed-map.json")

    assert(handler.errors.lengthCompare(1) == 0)
    val error = handler.errors.head
    assert(error.error.getMessage.startsWith("Syntax error : Missing closing map"))
    assert(handler.errors.head.inputRange.equals(InputRange(3, 57, 3, 57)))

  }

  test("Parse map entries separeted with break line") {
    val handler = getErrorsFor(jsonDir / "diffline-map-entries.json")
    assert(handler.errors.lengthCompare(0) == 0)
  }

  test("Parse map with only scalar") {
    val handler = getErrorsFor(jsonDir / "map-with-scalar.json")
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error : Expected ':' found '}'"))

  }

  test("Parse entry without separator") {
    val handler = getErrorsFor(jsonDir / "entry-without-indicator.json")
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error : Expected ':' found '\"MUA\",'"))
  }

  test("Test map in map closing range") { // todo: move to another test
    val jsonFile = jsonDir / "map-in-map.json"
    val parts = JsonParser(jsonFile.read()).parse()
    val restEntry = parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.head.value.as[YMap].entries.head
    assert(restEntry.key.as[YScalar].text == "RestEntity")
    val map = restEntry.value.as[YMap]
    assert(map.range.lineTo == 7)
    assert(map.range.columnTo == 5)
  }

  test("Parse entry with number") {
    val handler = getErrorsFor(jsonDir / "entry-with-number.json")
    assert(handler.errors.lengthCompare(2) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error in the following text: '{\n    \"start\" '"))
    assert(handler.errors.last.error.getMessage.startsWith("Syntax error in the following text: '}'"))
  }

  test("Parse unquoted key map") {
    val handler = getErrorsFor(jsonDir / "unquoted-keymap.json")
    assert(handler.errors.lengthCompare(12) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error in the following text: ''otherApplication1''"))
    assert(handler.errors(1).error.getMessage.startsWith("Syntax error in the following text: 'name'"))
  }

  test("Parse invalid number value") {
    val handler = getErrorsFor(jsonDir / "invalid-number.json")
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error in the following text: '0041533193'"))
  }

  test("Parse map with missing comma") {
    val handler = getErrorsFor(jsonDir / "map-without-comma.json")
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error : Expected ',' or '}' but found '\"'"))
  }

  test("Parse indicator after main seq close") {
    val handler = getErrorsFor(jsonDir / "bad-indicator-after-seq.json")
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error in the following text: ':'"))
  }

  test("Parse missing last mapentry value") {
    val handler = getErrorsFor(jsonDir / "missing-last-mapentry-value.json")
    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.startsWith("Syntax error : Expected value found '}'"))
  }

  // todo: generad valid jsons test
  test("Check text with spaces at begin") {
    val handler = getErrorsFor(jsonDir / "space-at-begin.json")
    assert(handler.errors.lengthCompare(0) == 0)
  }
  private def getErrorsFor(jsonFile:SyncFile): TestErrorHandler = {
    val handler = TestErrorHandler()

    val parts = JsonParser(jsonFile.read())(handler).parse()
    handler
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(node: YPart, e: SyamlException): Unit = errors += ErrorContainer(e, node.range)
  }

}

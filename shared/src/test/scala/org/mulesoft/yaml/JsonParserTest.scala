package org.mulesoft.yaml

import org.mulesoft.common.io.{Fs, SyncFile}
import org.mulesoft.lexer.{InputRange, SourceLocation}
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model._
import org.yaml.parser.JsonParser

import scala.collection.mutable

/**
  * Test handling errors
  */
trait JsonParserTest extends FunSuite with Matchers {

  private val jsonDir = Fs syncFile "shared/src/test/data/invalid-json"

  test("Parse unquoted key") {
    val errors = getErrorsFor(jsonDir / "unquoted-key.json")

    errors.length shouldBe 1
    assert(errors.head.error.getMessage.equals("Syntax error : Expecting '\"' but 'abb' found"))
    assert(errors.head.inputRange.equals(InputRange(3, 2, 3, 5)))

  }

  test("Parse unquoted value in seq") {
    val errors = getErrorsFor(jsonDir / "unquoted-value-seq.json")

    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.equals("Syntax error : Unexpected 'invalid'"))
    assert(errors.head.inputRange.equals(InputRange(3, 10, 3, 17)))
  }

  test("Parse scalar in map entry in map") {
    val errors = getErrorsFor(jsonDir / "scalar-in-map-entry.json")

    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.equals("Syntax error : Expecting '\"' but '#an invalid json comment only to check the raw render' found"))
    assert(errors.head.inputRange.equals(InputRange(5, 4, 5, 57)))
  }

  test("Parse unclosed map") {
    val errors = getErrorsFor(jsonDir / "unclosed-map.json")

    assert(errors.lengthCompare(1) == 0)
    val error = errors.head
    assert(error.error.getMessage.startsWith("Syntax error : Missing '}'"))
    assert(errors.head.inputRange.equals(InputRange(3, 57, 3, 57)))

  }

  test("Parse map entries separated with break line") {
    val errors = getErrorsFor(jsonDir / "diffline-map-entries.json")
    assert(errors.lengthCompare(0) == 0)
  }

  test("Parse map with only scalar") {
    val errors = getErrorsFor(jsonDir / "map-with-scalar.json")
    assert(errors.lengthCompare(3) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Expecting ':' but '}' found"))

  }

  test("Parse entry without separator") {
    val errors = getErrorsFor(jsonDir / "entry-without-indicator.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Missing ':'"))
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
    val errors = getErrorsFor(jsonDir / "entry-with-number.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Expecting ':' but '{' found"))
  }

  test("Parse unquoted key map") {
    val errors = getErrorsFor(jsonDir / "unquoted-keymap.json")
    assert(errors.lengthCompare(12) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Expecting '\"' but 'name' found"))
    assert(errors(1).error.getMessage.startsWith("Syntax error : Unexpected ''otherApplication1''"))
  }

  test("Parse invalid number value") {
    val errors = getErrorsFor(jsonDir / "invalid-number.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error in the following text: '0041533193'"))
  }

  test("Parse map with missing comma") {
    val errors = getErrorsFor(jsonDir / "map-without-comma.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Missing ','"))
  }

  test("Parse indicator after main seq close") {
    val errors = getErrorsFor(jsonDir / "bad-indicator-after-seq.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Unexpected ':'"))
  }

  test("Parse missing last mapentry value") {
    val errors = getErrorsFor(jsonDir / "missing-last-mapentry-value.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Unexpected '}'"))
  }

  test("Parse trailing comma in sequence") {
    val errors = getErrorsFor(jsonDir / "trailing-comma-seq.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Expecting 'value' but ']' found"))
  }

  test("Parse trailing comma in map") {
    val errors = getErrorsFor(jsonDir / "trailing-comma-map.json")
    assert(errors.lengthCompare(1) == 0)
    assert(errors.head.error.getMessage.startsWith("Syntax error : Expecting 'value' but '}' found"))
  }

  // todo: generad valid jsons test
  test("Check text with spaces at begin") {
    val errors = getErrorsFor(jsonDir / "space-at-begin.json")
    assert(errors.lengthCompare(0) == 0)
  }

  test("unexpected-tokens") {
    doTest("{ \"a\" : [1, 2, 3", "Syntax error : Missing ']'", 2)
    doTest("{ {", "Syntax error : Expecting '\"' but '{' found", 2)
    doTest("{ [", "Syntax error : Expecting '\"' but '[' found", 2)
    doTest("{ ]", "Syntax error : Expecting '\"' but ']' found", 2)
  }

  test("Duplicate keys don't throw exception with default error handler") {
    val source = """{"a": "aValue", "b": 1, "a": "anotherValue"}"""
    val testErrorHandler = TestErrorHandler()
    val handler = new DefaultJsonErrorHandler {
      override val errorHandler: ParseErrorHandler = testErrorHandler
    }
    JsonParser(source)(handler).parse()
    testErrorHandler.errors.size shouldBe 0
  }

  test("JsonErrorHandler notifies of Duplicate Keys in JSON") {
    val source = """{"a": "aValue", "b": 1, "a": "anotherValue"}"""
    val testErrorHandler = TestErrorHandler()
    val handler = new DefaultJsonErrorHandler {
      override protected def onIgnoredException(location: SourceLocation, e: SyamlException): Unit = testErrorHandler.handle(location, e)
    }
    JsonParser(source)(handler).parse()
    testErrorHandler.errors.head.error.getMessage shouldBe "Duplicate key : 'a'"
  }

  test("test recovery bad entry 1"){
    val jsonFile = jsonDir / "incomplete-entry-1.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 1)
    assert(eh.errors.size == 1)
  }

  test("test recovery bad entry 2"){
    val jsonFile = jsonDir / "incomplete-entry-2.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 3)
    assert(eh.errors.size == 1)
  }

  test("test recovery bad entry 3"){
    val jsonFile = jsonDir / "incomplete-entry-3.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({ case d: YDocument => d }).get.as[YMap].entries.length == 2)
    assert(eh.errors.size == 3)
  }

  test("test recovery bad entry 4"){
    val jsonFile = jsonDir / "incomplete-entry-4.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 2)
    assert(eh.errors.size == 5)
  }

  test("test recovery bad entry 5"){
    val jsonFile = jsonDir / "incomplete-entry-5.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 1)
    assert(eh.errors.size == 3)
  }

  test("test recovery bad entry 6"){
    val jsonFile = jsonDir / "incomplete-entry-6.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 2)
    assert(eh.errors.size == 1)
  }

  test("test recovery bad entry 7"){
    val jsonFile = jsonDir / "incomplete-entry-7.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 2)
    assert(eh.errors.size == 3)
  }

  test("test recovery bad value in seq 1"){
    val jsonFile = jsonDir / "incomplete-value-seq-1.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YSequence].nodes.length == 3)
    assert(eh.errors.size == 3)
  }

  test("test recovery bad map 1"){
    val jsonFile = jsonDir / "incomplete-map-1.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 1)
    assert(eh.errors.size == 1)
  }

  test("test recovery bad map 2"){
    val jsonFile = jsonDir / "incomplete-map-2.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 1)
    assert(eh.errors.size == 1)
  }

  test("test recovery bad seq 1"){
    val jsonFile = jsonDir / "incomplete-seq-1.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 1)
    assert(eh.errors.size == 5)
  }

  test("test recovery bad seq 2"){
    val jsonFile = jsonDir / "incomplete-seq-2.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 2)
    assert(eh.errors.size == 11)
  }

  test("test recovery bad seq 3"){
    val jsonFile = jsonDir / "incomplete-seq-3.json"
    val eh = TestErrorHandler()
    val parts = JsonParser(jsonFile.read())(eh).parse()
    assert(parts.collectFirst({case d:YDocument => d}).get.as[YMap].entries.length == 1)
    assert(eh.errors.size == 1)
  }

  private def doTest(source: String, msg: String, n: Int): Any = {
    val errors = getErrorsFor(source)
    assert(errors.lengthCompare(n) == 0)
    assert(errors.head.error.getMessage.startsWith(msg))
  }

  private def getErrorsFor(jsonFile:SyncFile): Seq[ErrorContainer] = {
    val handler = TestErrorHandler()

    JsonParser(jsonFile.read())(handler).parse()
    handler.errors
  }

  private def getErrorsFor(source: String): Seq[ErrorContainer] = {
    val handler = TestErrorHandler()

    JsonParser(source)(handler).parse()
    handler.errors
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()
    override def handle(loc: SourceLocation, e: SyamlException): Unit = errors += ErrorContainer(e, loc.inputRange)
  }
  case class ErrorContainer(error: Exception, inputRange: InputRange)

}

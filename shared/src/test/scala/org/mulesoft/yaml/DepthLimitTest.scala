package org.mulesoft.yaml

import org.mulesoft.lexer.BaseLexer
import org.mulesoft.test.TestErrorHandler
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.yaml.model.DepthLimitException
import org.yaml.parser.{JsonParser, YamlParser}

class DepthLimitTest extends AnyFunSuite with BeforeAndAfterEach {

  implicit var errorHandler: TestErrorHandler = _

  private val nestedDepthYAML: String =
    """{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{
      |[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{
      |{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{
      |{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{
      |{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{
      |{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[
      |{{{{{[{{{{{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{
      |[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{
      |{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{
      |{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{
      |{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{
      |{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[
      |{{{{{[{{{{{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{
      |[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{
      |{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{
      |{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{
      |{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{
      |{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[
      |{{{{{[{{{{{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{
      |[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{
      |{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{
      |{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{{{{{{{[{""".stripMargin

  private val nestedDepthJSON: String =
    """[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
      |[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[""".stripMargin

  // Small JSON/YAML with flows with depth = 5
  private val smallWithFlows: String =
    """{
      |  "A1": {
      |    "B2": {
      |      "C3": [
      |        {
      |          "D5": "something"
      |        }
      |      ]
      |    }
      |  },
      |  "E1": {
      |    "F2": {
      |      "G3": "other thing"
      |    },
      |    "H2": "aaa"
      |  }
      |}""".stripMargin

  override def beforeEach(): Unit = {
    errorHandler = TestErrorHandler()
  }

  test("YAML nested depth limitation test") {

    YamlParser(nestedDepthYAML).parse()

    errorHandler.errors.size shouldBe 1
    errorHandler.errors.head.error.isInstanceOf[DepthLimitException] shouldBe true
    errorHandler.errors.head.error.getMessage shouldBe (s"Reached maximum depth value of ${BaseLexer.DEFAULT_MAX_DEPTH}")

  }

  test("JSON nested depth limitation test") {

    JsonParser(nestedDepthJSON).parse()

    errorHandler.errors.size shouldBe 1
    errorHandler.errors.head.error.isInstanceOf[DepthLimitException] shouldBe true
    errorHandler.errors.head.error.getMessage shouldBe (s"Reached maximum depth value of ${BaseLexer.DEFAULT_MAX_DEPTH}")

  }

  test("Nested depth limitation test changing default value") {

    val limit: Int = 500

    YamlParser(nestedDepthYAML, Some(limit)).parse()

    errorHandler.errors.size shouldBe 1
    errorHandler.errors.head.error.isInstanceOf[DepthLimitException] shouldBe true
    errorHandler.errors.head.error.getMessage shouldBe (s"Reached maximum depth value of $limit")

  }

  test("Nested depth limitation test with small JSON - No Error") {

    val limit: Int = 5

    JsonParser(smallWithFlows, Some(limit)).parse()

    errorHandler.errors.size shouldBe 0

  }

  test("Nested depth limitation test with small JSON - Error") {

    val limit: Int = 4

    JsonParser(smallWithFlows, Some(limit)).parse()

    errorHandler.errors.size shouldBe 1
    errorHandler.errors.head.error.isInstanceOf[DepthLimitException] shouldBe true
    errorHandler.errors.head.error.getMessage shouldBe (s"Reached maximum depth value of $limit")

  }

  test("Nested depth limitation test with small Yaml with flows - No Error") {

    // YamlLexer with its logic of "try and restore" could generate a non-existent extra depth (it will remove it after that, but it could trigger the validation). It is never more than 1 extra.
    val limit: Int = 6

    YamlParser(smallWithFlows, Some(limit)).parse()

    errorHandler.errors.size shouldBe 0

  }

  test("Nested depth limitation test with small Yaml with flows - Error") {

    val limit: Int = 4

    YamlParser(smallWithFlows, Some(limit)).parse()

    errorHandler.errors.size shouldBe 1
    errorHandler.errors.head.error.isInstanceOf[DepthLimitException] shouldBe true
    errorHandler.errors.head.error.getMessage shouldBe (s"Reached maximum depth value of $limit")

  }

}
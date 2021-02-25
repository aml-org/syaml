package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model._
import org.yaml.parser.{JsonParser, YamlParser}
import org.yaml.render.{JsonRender, JsonRenderOptions, YamlRender, YamlRenderOptions}

/**
  * Test Extractors and exception Handling
  */
trait RenderingTest extends FunSuite with Matchers {

  test("Simple Document") {
    testDoc(
        """# Simple list
              |# Very simple
              |- 100 # A Number
              |- 123456789
              |- Plain Text
              |- "Quoted Text"
              |- |
              |  A Text
              |  With
              |  Several Lines
              |-
              |  k1: v1
              |  k2: v2
              |-
              |  - Nested
              |  - true
              |  - null
              |""".stripMargin,
        """[
              |  100,
              |  123456789,
              |  "Plain Text",
              |  "Quoted Text",
              |  "A Text\nWith\nSeveral Lines\n",
              |  {
              |    "k1": "v1",
              |    "k2": "v2"
              |  },
              |  [
              |    "Nested",
              |    true,
              |    null
              |  ]
              |]
              |""".stripMargin
    )

  }
  test("Simple Map") {
    testDoc(
        """# Simple list
              |# Very simple
              |number: 100 # A Number
              |qtext: "Quoted text"
              |ltext: |
              |  A Text
              |  With
              |  Several Lines
              |""".stripMargin,
        """{
              |  "number": 100,
              |  "qtext": "Quoted text",
              |  "ltext": "A Text\nWith\nSeveral Lines\n"
              |}
              |""".stripMargin
    )

  }
  test("Simple Literal") {
    testDoc(
        """# A Literal
              || # Here we go
              |  This is a literal
              |  spawning several
              |  lines
              |""".stripMargin,
        "\"This is a literal\\nspawning several\\nlines\\n\"\n"
    )
  }
  private def yaml1 =
    """emptyMap: {}
          |emptyEntry:
          |nullEntry: null
          |emptySeq: []
          |entry: something
          |""".stripMargin

  private def json1 =
    """{
          |  "emptyMap": {},
          |  "emptyEntry": null,
          |  "nullEntry": null,
          |  "emptySeq": [],
          |  "entry": "something"
          |}
          |""".stripMargin

  test("Empty Stuff") {
    testDoc(yaml1, json1)
  }

  test("Manually Built") {
    val doc = YDocument.obj(
        emptyMap = YMap.empty,
        emptyEntry = YNode.Empty,
        nullEntry = YNode.Null,
        emptySeq = YSequence.empty,
        entry = "something"
    )
    testDoc(doc, yaml1, json1)
  }

  test("Yaml Initial indentation") {
    val text =
      """aKey:
        |  anotherKey:
        |    - scalar""".stripMargin

    val parts = YamlParser(text).parse(false)
    val map:YMap = parts.collectFirst({case d:YDocument => d}).get.node.as[YMap]

    val str = YamlRender.render(map, 4)
    val expected = text.split("\n").map("    " + _).mkString("\n")
    str shouldBe expected
  }

  test("Json Initial indentation") {
    val text =
      """{
        |  "aKey": {
        |    "anotherKey": [
        |      "scalar"
        |    ]
        |  }
        |}""".stripMargin

    val expected =
      """{
        |      "aKey": {
        |        "anotherKey": [
        |          "scalar"
        |        ]
        |      }
        |    }
        |""".stripMargin

    val parts = JsonParser(text).parse(false)
    val doc:YDocument = parts.collectFirst({case d:YDocument => d}).get

    val str = JsonRender.render(doc, 4)
    str shouldBe expected
  }

  test("Json render options - spacesInTab") {
    val text =
      """{
        |  "aKey": {
        |    "anotherKey": [
        |      "scalar"
        |    ]
        |  }
        |}""".stripMargin

    val expected4spaces =
      """{
        |    "aKey": {
        |        "anotherKey": [
        |            "scalar"
        |        ]
        |    }
        |}
        |""".stripMargin

    val parts = JsonParser(text).parse(false)
    val doc:YDocument = parts.collectFirst({case d:YDocument => d}).get

    val output4spaces = JsonRender.render(doc, 0, new JsonRenderOptions().withIndentationSize(4))
    output4spaces shouldBe expected4spaces
  }

  test("Json render options - preferSapces false") {
    val text =
      """{
        |  "aKey": {
        |    "anotherKey": [
        |      "scalar"
        |    ]
        |  }
        |}""".stripMargin


    val expectedTabs =
      s"""{
         |\t"aKey": {
         |\t\t"anotherKey": [
         |\t\t\t"scalar"
         |\t\t]
         |\t}
         |}
         |""".stripMargin

    val parts = JsonParser(text).parse(false)
    val doc:YDocument = parts.collectFirst({case d:YDocument => d}).get

    val outputTabs = JsonRender.render(doc, 0, new JsonRenderOptions().withPreferSpaces(false))
    outputTabs shouldBe expectedTabs
  }

  test("Yaml render options - indentation size") {
    val text =
      """aKey:
        |  anotherKey:
        |    - scalar
        |  new: val""".stripMargin

    val expected1space =
      """aKey:
        | anotherKey:
        |  - scalar
        | new: val
        |""".stripMargin

    val expected4spaces =
      """aKey:
        |    anotherKey:
        |        - scalar
        |    new: val
        |""".stripMargin

    val parts         = YamlParser(text).parse(false)
    val output1space  = YamlRender.render(parts, expandReferences = false, new YamlRenderOptions().withIndentationSize(1), 0)
    val output4spaces = YamlRender.render(parts, expandReferences = false, new YamlRenderOptions().withIndentationSize(4), 0)

    output4spaces shouldBe expected4spaces
    output1space shouldBe expected1space

  }

  private def testDoc(text: String, jsonText: String): Unit = {
    val parts = YamlParser(text).parse()
    val str   = YamlRender.render(parts)
    str shouldBe text

    val doc = YamlParser(text).document()
    testDoc(doc, text, jsonText)
  }

  private def testDoc(doc: YDocument, text: String, jsonText: String): Unit = {
    val strDoc = YamlRender.render(doc)
    strDoc shouldBe text

    val json = JsonRender.render(doc)
    json shouldBe jsonText
  }

  test("Yaml flow render - mixed flow and block map") {
    val text =
      """p1: {
        |  key: value,
        |  map: {
        |    k1: v,
        |    k2: v
        |  },
        |  brother: value
        |}
        |p2:
        |   k:
        |     k1: v1
        |     k2: {
        |       in: flow
        |     }""".stripMargin

    val expected =
      """{
        |  p1: {
        |    key: value,
        |    map: {
        |      k1: v,
        |      k2: v
        |    },
        |    brother: value
        |  },
        |  p2: {
        |    k: {
        |      k1: v1,
        |      k2: {
        |        in: flow
        |      }
        |    }
        |  }
        |}""".stripMargin

    val parts   = YamlParser(text).parse(false)
    val mainMap = parts.collectFirst({case d: YDocument => d}).get.as[YMap]
    val output  = YamlRender.render(mainMap.toFlow, expandReferences = false)

    output shouldBe expected
  }

  test("Yaml flow render - map test") {
    val text =
      """p1:
        |  anotherKey: scalar
        |  key:
        |    k1: v1
        |    k2: v2
        |  new: val
        |p2:
        |   k:
        |     k1: v1
        |     k2: v2""".stripMargin

    val expected =
      """{
        |  p1: {
        |    anotherKey: scalar,
        |    key: {
        |      k1: v1,
        |      k2: v2
        |    },
        |    new: val
        |  },
        |  p2: {
        |    k: {
        |      k1: v1,
        |      k2: v2
        |    }
        |  }
        |}""".stripMargin

    val parts         = YamlParser(text).parse(false)
    val mainMap = parts.collectFirst({case d: YDocument => d}).get.as[YMap]
    val output = YamlRender.render(mainMap.toFlow, expandReferences = false)

    output shouldBe expected

  }

  test("Yaml flow render - sequence test") {
    val text =
      """- a
        |- b
        |- c
        |- d""".stripMargin

    val expected =
      """[
        |  a,
        |  b,
        |  c,
        |  d
        |]""".stripMargin

    val parts         = YamlParser(text).parse(false)
    val mainSeq = parts.collectFirst({case d: YDocument => d}).get.as[YSequence]
    val output = YamlRender.render(Seq(mainSeq.toFlow), expandReferences = false, new YamlRenderOptions(), 0)

    output shouldBe expected

  }

  test("Yaml flow render - full test") {
    val text =
      """key:
        |  subkey1:
        |   - enum
        |   - with
        |   - scalars
        |  subkey2:
        |   - enum:
        |       with: map
        |   - another:
        |       enum:
        |         with: map
        |       brother: val
        |  subkey3:
        |    map:
        |      key: [enum, enum2]
        |      key2: val
        |      key3: val""".stripMargin

    val expected =
      """{
        |  key: {
        |    subkey1: [
        |      enum,
        |      with,
        |      scalars
        |    ],
        |    subkey2: [
        |      {
        |        enum: {
        |          with: map
        |        }
        |      },
        |      {
        |        another: {
        |          enum: {
        |            with: map
        |          },
        |          brother: val
        |        }
        |      }
        |    ],
        |    subkey3: {
        |      map: {
        |        key: [
        |          enum,
        |          enum2
        |        ],
        |        key2: val,
        |        key3: val
        |      }
        |    }
        |  }
        |}""".stripMargin

    val parts         = YamlParser(text).parse(false)
    val mainMap = parts.collectFirst({case d: YDocument => d}).get.as[YMap]
    val output = YamlRender.render(mainMap.toFlow, expandReferences = false)

    output shouldBe expected

  }

  implicit class yMapConverter(ymap: YMap) {
    def toFlow: YMap = YMap(ymap.location, ymap.children, inFlow = true)
  }

  implicit class ySeqConverter(yseq: YSequence) {
    def toFlow: YSequence = YSequence(yseq.location, yseq.children, inFlow = true)
  }
}

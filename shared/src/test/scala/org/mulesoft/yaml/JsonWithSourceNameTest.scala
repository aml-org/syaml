package org.mulesoft.yaml

import org.yaml.parser.JsonParser

/**
  * Test against golden files
  */
trait JsonWithSourceNameTest extends WithSourceNameTest {

  private val rootMap =
    """{
      | "map": {
      |   "entry1": "a",
      |   "entry2": "b",
      |   "entry3": {
      |     "map": {
      |       "entry4": "c",
      |       "entry5": [ "a","b",{"entry6": "d"},{"map": {"entry7": "e"}} ]
      |     }
      |   }
      | }
      |}
    """.stripMargin

  private val rootSeq =
    """
      |[ {
      |     "entry1": "a",
      |     "entry2": {
      |       "map": {
      |         "entry3": "b",
      |         "entry4": ["a","b"]
      |       },
      |     "entry3":[ "a", "b" ]
      |     }
      |  },
      | "b" ]
    """.stripMargin


  private val rootScalar = "\"scalar\""
  override protected val sourceName = "sourcename.json"

  test("assert source name root map"){
    val document = JsonParser.withSource(rootMap,sourceName).documents().head
    assertNameInChild(document)
  }

  test("assert source name root seq"){
    val document = JsonParser.withSource(rootSeq,sourceName).documents().head
    assertNameInChild(document)
  }

  test("assert source name root scalar"){
    val document = JsonParser.withSource(rootScalar,sourceName).documents().head
    assertNameInChild(document)
  }

  test("assert empty source name root map"){
    val document = JsonParser.withSource(rootMap,"").documents().head
    assertEmptySourceName(document)
  }
}
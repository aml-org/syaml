package org.mulesoft.yaml

import org.mulesoft.test.GoldenSuite
import org.yaml.model._
import org.yaml.parser.YamlParser

trait YamlParserTest extends GoldenSuite {

  private val yamlDir = fs.syncFile("shared/src/test/data/yaml")

  test("Parsed YSequence marked as flow") {
    val yaml = "example-7.13.yaml"
    val yamlFile   = fs.syncFile(yamlDir, yaml)
    val parts = YamlParser(yamlFile.read()).parse()
    val mainSequence = parts.collectFirst({case d: YDocument => d}).get.as[YSequence]
    assert(!mainSequence.isInFlow)
    val childSequences : IndexedSeq[YSequence] = mainSequence.children.collect({case y: YNodePlain => y.value}).map(_.asInstanceOf[YSequence])
    assert(childSequences.size == 2)
    assert(allFlow(childSequences))
  }

  test("Parsed curly bracket in scalar should not mark as flow") {
    val yaml = "example-5.12.yaml"
    val yamlFile   = fs.syncFile(yamlDir, yaml)
    val parts = YamlParser(yamlFile.read()).parse()
    noFlow(parts)
  }

  test("Block sequence shouldn't be marked as flow") {
    val yaml = "example-8.14.yaml"
    val yamlFile   = fs.syncFile(yamlDir, yaml)
    val parts = YamlParser(yamlFile.read()).parse()
    noFlow(parts)
  }

  test("Flow sequence in block map should be marked as flow") {
    val yaml = "example-7.11.yaml"
    val yamlFile   = fs.syncFile(yamlDir, yaml)
    val parts = YamlParser(yamlFile.read()).parse()
    val mainMap = parts.collectFirst({case d: YDocument => d}).get.as[YMap]
    assert(!mainMap.isInFlow)
    val childSequences : IndexedSeq[YValue] = mainMap.children.collect({case y: YNodePlain => y.value})
    assert(allFlow(childSequences))
  }

  test("Yaml parsed flow in mixed maps") {
    val text =
      """p1: {
        |  key: value,
        |  map: {
        |    k1: v,
        |    k2: [a, b, c]
        |  },
        |  brother: value
        |}
        |p2:
        |   k:
        |     k1: v1
        |     k2: {
        |       in: flow
        |     }
        |   flowSeq: [a, b, c]
        |p3:
        |  map:
        |    k1: v1
        |    k2: v2
        |    enum:
        |     - a
        |     - b
        |  enum:
        |   - some
        |   - random
        |   - values""".stripMargin

    val parts = YamlParser(text).parse()
    val mainMap = parts.collectFirst({case d: YDocument => d}).get.as[YMap]
    assert(!mainMap.isInFlow)
    assert(mainMap.entries.size == 3)

    val p1 = mainMap.entries(0)
    allFlow(p1.children)

    val p2Map = mainMap.entries(1).value.as[YMap]
    val k =  p2Map.entries(0).value.as[YMap]
    val flowSeq = p2Map.entries(1)
    allFlow(flowSeq)
    val k1 = k.entries(0)
    val k2 =k.entries(1)
    noFlow(k1)
    allFlow(k2)

    val p3 = mainMap.entries(2)
    noFlow(p3.children)
    val childSequences : IndexedSeq[YValue] = mainMap.children.collect({case y: YNodePlain => y.value})
    assert(allFlow(childSequences))
  }

  private def allFlow(ypart: YPart): Boolean = allFlow(IndexedSeq(ypart))

  private def allFlow(parts: IndexedSeq[YPart]): Boolean = {
    parts.forall({
      case m: YMap => m.isInFlow && allFlow(m.children)
      case s: YSequence => s.isInFlow && allFlow(s.children)
      case e => allFlow(e.children)
    })
  }

  private def noFlow(ypart: YPart): Boolean = noFlow(IndexedSeq(ypart))

  private def noFlow(parts: IndexedSeq[YPart]): Boolean = {
    parts.forall({
      case m: YMap => !m.isInFlow && noFlow(m.children)
      case s: YSequence => !s.isInFlow && noFlow(s.children)
      case e => noFlow(e.children)
    })
  }
}

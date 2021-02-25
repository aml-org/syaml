package org.mulesoft.yaml

import org.mulesoft.test.GoldenSuite
import org.yaml.model.{YDocument, YNodePlain, YSequence}
import org.yaml.parser.YamlParser

trait YamlParserTest extends GoldenSuite {

  private val yamlDir = fs.syncFile("shared/src/test/data/yaml")

  test("Parse YParts in flow") {
    val yaml = "example-7.13.yaml"
    val yamlFile   = fs.syncFile(yamlDir, yaml)
    val parts = YamlParser(yamlFile.read()).parse()
    val mainSequence = parts.collectFirst({case d: YDocument => d}).get.as[YSequence]
    assert(!mainSequence.isInFlow)
    val childSequences : Seq[YSequence] = mainSequence.children.collect({case y: YNodePlain => y.value}).map(_.asInstanceOf[YSequence])
    assert(childSequences.size == 2)
    assert(childSequences.forall(_.isInFlow))
  }
}

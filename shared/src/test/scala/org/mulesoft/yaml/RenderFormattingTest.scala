package org.mulesoft.yaml

import org.mulesoft.common.test.Diff
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.model.YPart
import org.yaml.parser.{JsonParser, YamlParser}
import org.yaml.render.{JsonRender, JsonRenderOptions, YamlRender, YamlRenderOptions}

trait RenderFormattingTest extends GoldenSuite {

  def testInMemory(parts: Seq[YPart], expected: String, indent: Int = 2): String = {
    val str = YamlRender.render(parts, YamlRenderOptions(indent, applyFormatting = true))
    str shouldBe expected
    str
  }

  private val modelDir  = mkdir("target", "test", "model")
  private val modelDir4Indent  = mkdir("target", "test", "model/indent-4")
  private val yamlDir         = fs syncFile "shared/src/test/data/formatting-render/yaml"
  private val goldenDir       = fs syncFile "shared/src/test/data/formatting-render/yaml/expected"
  private val goldenDir4Indent       = fs syncFile "shared/src/test/data/formatting-render/yaml/expected-indent-4"
  private val jsonDir         = fs syncFile "shared/src/test/data/formatting-render/json"
  private val goldenJsonDir       = fs syncFile "shared/src/test/data/formatting-render/json/expected"
  private val goldenJsonDir4Indent       = fs syncFile "shared/src/test/data/formatting-render/json/expected-indent-4"

  private val files = yamlDir.list.filter(_ endsWith "yaml")
  private val jsonFiles = jsonDir.list.filter(_ endsWith "json")

  for (yaml <- files) {
    test("YAML - Render with formatting " + yaml) {
      val yamlFile       = fs.syncFile(yamlDir, yaml)
      val output         = fs.syncFile(modelDir, yaml)
      val goldenFile     = fs.syncFile(goldenDir, yaml)
      val output4         = fs.syncFile(modelDir4Indent, yaml)
      val goldenFile4     = fs.syncFile(goldenDir4Indent, yaml)

      generateYaml(yamlFile, output, 2)
      generateYaml(yamlFile, output4, 4)

      doDeltas(output, goldenFile)
      doDeltas(output4, goldenFile4)

    }
  }

  for (json <- jsonFiles) {
    test("JSON - Render with formatting " + json) {
      val jsonFile       = fs.syncFile(jsonDir, json)
      val output         = fs.syncFile(modelDir, json)
      val goldenFile     = fs.syncFile(goldenJsonDir, json)
      val output4         = fs.syncFile(modelDir4Indent, json)
      val goldenFile4     = fs.syncFile(goldenJsonDir4Indent, json)

      generateJson(jsonFile, output, 2)
      generateJson(jsonFile, output4, 4)

      doDeltas(output, goldenFile)
      doDeltas(output4, goldenFile4)

    }
  }

  private def generateYaml(yamlFile: SyncFile, output: SyncFile, indentation: Int): Unit =
    output.write(YamlRender.render(YamlParser(yamlFile.read()).parse(), YamlRenderOptions(indentation, applyFormatting = true)))

  private def generateJson(jsonFile: SyncFile, output: SyncFile, indentation: Int): Unit = {
    val document = JsonParser(lines(jsonFile).mkString("\n")).document(true)
    output.write(JsonRender.render(document, 0, JsonRenderOptions(indentation, preferSpaces = true, applyFormatting = true)))
  }

  override def doDeltas(yeastFile: SyncFile, goldenFile: SyncFile): Unit = {
    val deltas = Diff.caseSensitive.diff(lines(yeastFile), lines(goldenFile))
    assert(deltas.isEmpty, s"diff -y -W 150 $yeastFile $goldenFile")
  }
}

package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.lexer.{YamlLexer, YamlToken}

/**
  * Test against golden files
  */
trait YamlToYeastTest extends GoldenSuite {

  private val yeastDir  = mkdir("target", "test", "yeast")
    private val yamlDir   = fs.syncFile("shared/src/test/data/yaml")
    private val goldenDir = fs.syncFile("shared/src/test/data/yeast")

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list else Array(file)
  for (yaml <- files) {
    test("Generate Yeast for " + yaml) {
      val yamlFile   = fs.syncFile(yamlDir, yaml)
      val yeast      = yaml replaceExtension (if (yaml endsWith "yaml") "yt" else "jyt")
      val yeastFile  = fs.syncFile(yeastDir, yeast)
      val goldenFile = fs.syncFile(goldenDir, yeast)

      generate(yamlFile, yeastFile)
      doDeltas(yeastFile, goldenFile)

    }
  }

  private def generate(yamlFile: SyncFile, yeastFile: SyncFile) = {
    val lexer = YamlLexer(yamlFile.read()).initialize()
    val out = new StringBuilder
    while (lexer.token != YamlToken.EndStream) {
      val data = YeastData(lexer.tokenData, lexer.tokenString)
      out append data append '\n'
      lexer.advance()
    }
    yeastFile write out
  }
}

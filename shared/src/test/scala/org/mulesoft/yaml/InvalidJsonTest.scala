package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.lexer.{JsonLexer, YamlToken}

/**
  * Created by pedro.colunga on 11/10/17.
  */
trait InvalidJsonTest extends GoldenSuite {

  private val yeastDir  = mkdir("target", "test", "yeast")
  private val yamlDir   = fs syncFile "shared/src/test/data/invalid-json"
  private val goldenDir = fs syncFile "shared/src/test/data/yeast/invalid-json"

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list else Array(file)

  for (yaml <- files) {
    if(yaml.endsWith(".ignore")){
      ignore("Invalid for "+yaml){
        succeed
      }
    }else{

      test("Invalid for " + yaml) {
        val yamlFile   = yamlDir / yaml
        val yeast      = yaml.replaceExtension(if (yaml endsWith "yaml") "yt" else "jyt")
        val yeastFile  = yeastDir / yeast
        val goldenFile = goldenDir / yeast

        generate(yamlFile, yeastFile)

        doDeltas(yeastFile, goldenFile)
      }
    }
  }

  private def generate(yamlFile: SyncFile, yeastFile: SyncFile) = {
    val out   = new StringBuilder
    val lexer = JsonLexer(yamlFile.read()).initialize()
    while (lexer.token != YamlToken.EndDocument) { // json files have only 1 document and ends with that token
      val data = YeastData(lexer.tokenData, lexer.tokenString)
      out.append(data).append('\n')
      lexer.advance()
    }
    yeastFile.write(out)
  }
}

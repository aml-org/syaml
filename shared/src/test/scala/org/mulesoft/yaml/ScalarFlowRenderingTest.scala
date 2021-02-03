package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.test.Diff
import org.scalatest.{Assertion, FunSuite, Matchers}
import org.yaml.model._
import org.yaml.render.YamlRender

/**
  * Test render all rules of scalar flows
  */
trait ScalarFlowRenderingTest extends FunSuite with Matchers {

  val cIndicators = Seq(",", "[", "]", "{", "}", "#", "&", "*", "!", "|", ">", "'", "\"", "%", "@", "`")
  val withSpaceIndicator = Seq("?", ":", "-")
  val flowIndicators = Seq("," , "[" , "]" , "{" , "}")
  val especialIndicators = Seq(":","#")

  /** Rule [126] ( ns-char - c-indicator ))*/
  test("Flow Scalars - ns-plain-first") {
    var yamlBuilder = StringBuilder.newBuilder
    val c = IndexedSeq()

    val doc = YDocument(b => {
      b.obj { pb =>
        cIndicators.foreach { i =>
          val str = i.encode
          yamlBuilder.append("e: \"" + str + "\"\n")
          pb.entry("e", i)

          yamlBuilder.append("e: \"" + str +"a"+ "\"\n")
          pb.entry("e", i+"a")
        }
      }
    })

    yamlBuilder.toString()
    assertYaml(doc, yamlBuilder.mkString)
  }

  /**
    * [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
    * +
    * [126]	ns-plain-first(c)	::=	  ( ns-char - c-indicator )
        | ( ( “?” | “:” | “-” )
    * non first non c-indicator
    */
  test("Flow Scalars - (cIndicators - flowIndicator) non first") {
    var yamlBuilder = StringBuilder.newBuilder
    val c = IndexedSeq()
    val indicators = cIndicators.filter(c => !flowIndicators.contains(c) && !Seq("#",":").contains(c))
    val doc = YDocument(b => {
      b.obj { pb =>
        indicators.foreach { i =>
          val str = i.encode
          yamlBuilder.append("e: a" + i + "\n")
          pb.entry("e", "a"+i)
          yamlBuilder.append("e: a " + i + "\n")
          pb.entry("e", "a "+i)
        }
      }
    })

    yamlBuilder.toString()
    assertYaml(doc, yamlBuilder.mkString)
  }

  /**
    * [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
    */
  test("Flow Scalars - flow indicator") {
    var yamlBuilder = StringBuilder.newBuilder
    var jsonBuilder = StringBuilder.newBuilder
    jsonBuilder.append("{\n")
    val c = IndexedSeq()

    val doc = YDocument(b => {
      b.obj { pb =>
        flowIndicators.foreach { i =>
          val str = i.encode
          yamlBuilder.append("e: " + "a" + i + "b" + "\n")
          pb.entry("e", "a"+i+"b")

          yamlBuilder.append("e: " + "a " + i + " b" + "\n")
          pb.entry("e", "a "+i+" b")

          yamlBuilder.append("e: " + "a " + i + "\n")
          pb.entry("e", "a "+i)

          yamlBuilder.append("e: " + "a" + i + "\n")
          pb.entry("e", "a"+i)

          yamlBuilder.append("e: \"" + str + " b" + "\"\n")
          pb.entry("e", i + " b")

          yamlBuilder.append("e: \"" + str + "b" + "\"\n")
          pb.entry("e", i + "b")
        }
      }
    })

    yamlBuilder.toString()
    assertYaml(doc, yamlBuilder.mkString)
  }

  /** Rule [126] ( ( “?” | “:” | “-” )
    * Followed by an ns-plain-safe(c))  )*/
  test("Flow Scalars - ( “?” | “:” | “-” ) - followed by ns-plain-safe") {
    var yamlBuilder = StringBuilder.newBuilder
    val c = IndexedSeq()

    val doc = YDocument(b => {
      b.obj { pb =>
        withSpaceIndicator.filter(c => !especialIndicators.contains(c)).foreach { i =>
          val str = i.encode
          yamlBuilder.append("e: \"" +  str + " b" + "\"\n")
          pb.entry("e", i+" b")

          yamlBuilder.append("e: " + str + "b" + "\n")
          pb.entry("e", i + "b")

          yamlBuilder.append("e: " + "b" + str + "a" + "\n")
          pb.entry("e", "b" + i + "a")

          yamlBuilder.append("e: " + "b " + str + " a" + "\n")
          pb.entry("e", "b " + i + " a")
        }
      }
    })

    yamlBuilder.toString()
    assertYaml(doc, yamlBuilder.mkString)
  }

  /**
    * [130]	ns-plain-char(c)	::=	  ( ns-plain-safe(c) - “:” - “#” )
        | ( “:” /* Followed by an ns-plain-safe(c) */ )
    */
  test("Flow Scalars - ns-plain-char(c) - :_ indicator") {
    var yamlBuilder = StringBuilder.newBuilder
    val c = IndexedSeq()

    val doc = YDocument(b => {
      b.obj { pb =>
        yamlBuilder.append("e: a:b\n")
        pb.entry("e", "a:b")

        yamlBuilder.append("e: \"a: b\"\n")
        pb.entry("e", "a: b")

        yamlBuilder.append("e: a :b\n")
        pb.entry("e", "a :b")

        yamlBuilder.append("e: \"a:\"\n")
        pb.entry("e", "a:")

        yamlBuilder.append("e: \": b\"\n")
        pb.entry("e", ": b")

        yamlBuilder.append("e: :b\n")
        pb.entry("e", ":b")

      }
    })

    yamlBuilder.toString()
    assertYaml(doc, yamlBuilder.mkString)
  }

  /**
    * [130]	ns-plain-char(c)	::=	  ( ns-plain-safe(c) - “:” - “#” )
        | ( /* An ns-char preceding */ “#” )
    */
  test("Flow Scalars - ns-plain-char(c) - _# indicator") {
    var yamlBuilder = StringBuilder.newBuilder
    val c = IndexedSeq()

    val doc = YDocument(b => {
      b.obj { pb =>
        withSpaceIndicator.foreach { i =>
          val str = i.encode
          yamlBuilder.append("e: a#b\n")
          pb.entry("e", "a#b")

          yamlBuilder.append("e: a# b\n")
          pb.entry("e", "a# b")

          yamlBuilder.append("e: \"a #b\"\n")
          pb.entry("e", "a #b")

          yamlBuilder.append("e: \"a #\"\n")
          pb.entry("e", "a #")

          yamlBuilder.append("e: a#\n")
          pb.entry("e", "a#")

          yamlBuilder.append("e: \"#b\"\n")
          pb.entry("e", "#b")

          yamlBuilder.append("e: \"# b\"\n")
          pb.entry("e", "# b")
        }
      }
    })

    yamlBuilder.toString()
    assertYaml(doc, yamlBuilder.mkString)
  }

  private def assertYaml(doc: YDocument, yaml: String): Assertion = assertRender(YamlRender.render(doc), yaml)

  private def assertRender(rendered: String, golden: String): Assertion = {
    val diffs = Diff.caseSensitive.diff(rendered, golden)
    if (diffs.nonEmpty) fail(Diff.makeString(diffs))
    else succeed
  }


}

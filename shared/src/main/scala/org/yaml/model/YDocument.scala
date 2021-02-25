package org.yaml.model

import org.mulesoft.lexer.SourceLocation
import org.yaml.convert.YRead
import org.yaml.parser.{JsonParser, YamlParser}

import scala.collection.mutable.ArrayBuffer
import scala.language.{dynamics, implicitConversions}

/**
  * A Yaml Document
  */
class YDocument(location: SourceLocation, children: IndexedSeq[YPart])
    extends YPart(location, children)
    with YNodeLike {

  /** The Main Document Node */
  val node: YNode    = children collectFirst { case a: YNode => a } getOrElse YNode.Null
  val tagType: YType = node.tag.tagType

  val headComment: String = children takeWhile (!_.isInstanceOf[YNode]) collect {
    case c: YComment => c.metaText
  } mkString "\n"

  override def toString: String = "Document: " + node.toString

  override def to[T: YRead]: Either[YError, T] = obj.to

  override def obj: YObj = if (node == YNode.Null) YFail(this, "Empty Document") else YSuccess(node)

  override protected[model] def thisNode: YNode = node

  override def equals(obj: scala.Any): Boolean = obj match {
    case doc: YDocument => doc.node.equals(this.node)
    case _              => false
  }
}

object YDocument {

  def apply(children: IndexedSeq[YPart], sourceName: String): YDocument =
    new YDocument(SourceLocation(sourceName), children)

  /** Build supplying a Head comment */
  def apply(headComment: String, sourceName: String): WithComment =
    new WithComment(if (headComment == null) "" else headComment, sourceName)

  def apply(headComment: String): WithComment =
    new WithComment(if (headComment == null) "" else headComment, "")

  /** Constructor from a Builder */
  def apply(f: PartBuilder => Unit, sourceName: String): YDocument = new WithComment("", sourceName)(f)

  def apply(f: PartBuilder => Unit): YDocument = YDocument("")(f)

  /** Constructor from a main Node */
  def apply(mainNode: YNode)(implicit sourceName: String = ""): YDocument = YDocument("", sourceName)(mainNode)

  /** Constructor from Yaml text, keep the first document */
  def parseYaml(text: String)(implicit eh: ParseErrorHandler): YDocument = YamlParser(text)(eh).document()

  /** Constructor from Json text */
  def parseJson(text: String)(implicit eh: ParseErrorHandler): YDocument = JsonParser(text)(eh).document()

  /** Build an Object(Map) (Using dynamics) */
  object obj extends Dynamic {
    def applyDynamicNamed(method: String)(args: (String, YNode)*)(implicit sourceName: String = ""): YNode =
      method match {
        case "apply" =>
          YNode.fromMap(
              YMap(args
                     .map { t =>
                       val key   = YNode(t._1, sourceName)
                       val value = if (t._2 eq null) YNode.Null else t._2
                       YMapEntry(key, value)
                     }
                     .toArray[YPart],
                   sourceName, inFlow = false))
      }
  }

  /** Build an Object document using a builder */
  def objFromBuilder(f: EntryBuilder => Unit): YDocument = YDocument("", "").objFromBuilder(f)

  /** Build an List document using a builder */
  def list(f: PartBuilder => Unit): YDocument = YDocument("", "").list(f)

  /** Build a list of Nodes */
  def list(elems: YNode*)(implicit sourceName: String = ""): YNode = YNode(
      YSequence(SourceLocation(sourceName), elems.toArray[YNode])
  )

  /** Convert from an node to a document */
  implicit def fromNode(node: YNode): YDocument = YDocument("", node.sourceName)(node)

  /** Auxiliary class to create a document that has a head comment */
  class WithComment(val comment: String, sourceName: String) {

    /** Constructor from a Builder */
    def apply(f: PartBuilder => Unit): YDocument = {
      val b = new PartBuilder(sourceName)
      if (comment.nonEmpty) b comment comment
      f(b)
      new YDocument(SourceLocation(sourceName), b.builder.result)
    }

    /** Constructor from a Head Comment and a main Node */
    def apply(mainNode: YNode): YDocument = createDoc(mainNode)

    /** Build from a list of Nodes */
    def list(elems: YNode*)(implicit sourceName: String = ""): YDocument =
      apply(YDocument.list(elems: _*)(sourceName))

    /** Build an Object (Using dynamics) */
    object obj extends Dynamic {
      def applyDynamicNamed(method: String)(args: (String, YNode)*): YDocument =
        createDoc(YDocument.obj.applyDynamicNamed(method)(args: _*)(sourceName))
    }

    /** Build an Object document using a builder */
    def objFromBuilder(f: EntryBuilder => Unit): YDocument = apply(createMapNode(f, sourceName))

    /** Build an List document using a builder */
    def list(f: PartBuilder => Unit): YDocument = apply(createSeqNode(f, sourceName))

    private def createDoc(mainNode: YNode) =
      new YDocument(
          SourceLocation(sourceName),
          if (comment.isEmpty) Array(mainNode) else Array(new YComment(comment, SourceLocation(sourceName)), mainNode))
  }
  abstract class BaseBuilder {
    private[YDocument] val builder = new ArrayBuffer[YPart]

    /** Add a Comment */
    def comment(text: String): Unit =
      for (line <- text split "\n") builder += new YComment(line, SourceLocation(sourceName))
    val sourceName: String
  }

  class PartBuilder(override val sourceName: String) extends BaseBuilder {

    /** Add a Node to the builder */
    def +=(node: YNode): Unit = builder += node

    /** Add a Scalar Integer to the builder */
    def +=(int: Int): Unit = builder += YNode(int, sourceName)

    /** Add a Scalar String to the builder */
    def +=(str: String): Unit = builder += YNode(str, sourceName)

    /** Add a List to the builder */
    def list(f: PartBuilder => Unit): Unit = builder += createSeqNode(f, sourceName)

    /** Add an object (aka map) to the builder */
    def obj(f: EntryBuilder => Unit): Unit = builder += createMapNode(f, sourceName)
  }

  class EntryBuilder(override val sourceName: String) extends BaseBuilder with Dynamic {
    final def complexEntry(kf: PartBuilder => Unit, vf: PartBuilder => Unit): Unit = {
      val k = new PartBuilder(sourceName)
      kf(k)
      val v = new PartBuilder(sourceName)
      vf(v)
      addEntry(k.builder(0).asInstanceOf[YNode], v.builder(0).asInstanceOf[YNode])
    }

    final def entry(key: YNode, vf: PartBuilder => Unit): Unit = {
      val v = new PartBuilder(sourceName)
      vf(v)
      v.builder.insert(0, key)
      builder += YMapEntry(v.builder)
    }

    final def entry(key: YNode, value: YNode): Unit = addEntry(key, value)

    final def updateDynamic(name: String)(value: YNode): Unit = addEntry(YNode(name, sourceName), value)

    private def addEntry(k: YNode, v: YNode): Unit = builder += YMapEntry(k, v)

  }

  private def createSeqNode(f: PartBuilder => Unit, sourceName: String) = {
    val b = new PartBuilder(sourceName)
    f(b)
    val node = YNode(YSequence(SourceLocation(sourceName), b.builder.result), YType.Seq)
    node
  }

  private def createMapNode(f: EntryBuilder => Unit, sourceName: String, inFlow: Boolean = false) = {
    val b = new EntryBuilder(sourceName)
    f(b)
    YNode(YMap(b.builder.result, sourceName, inFlow), YType.Map)
  }

}

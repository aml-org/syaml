package org.yaml.builder
import org.yaml.builder.DocBuilder.SType._
import org.yaml.builder.DocBuilder._
import org.yaml.model._

import scala.collection.mutable.ArrayBuffer

class YDocumentBuilder extends DocBuilder[YPart] {

  private var _document: Option[YPart] = None
  def document: YPart                  = _document.get
  def document_=(doc: YPart): Unit     = _document = Some(doc)

  override def isDefined: Boolean = _document.isDefined
  override def result: YPart      = document

  override def list(f: Part[YPart] => Unit): YPart = {
    val doc = YDocument.fromNode(createSeqNode(f))
    _document = Some(doc)
    doc
  }

  override def obj(f: Entry[YPart] => Unit): YPart = {
    val doc = YDocument.fromNode(createMapNode(f))
    _document = Some(doc)
    doc
  }

  override def doc(f: Part[YPart] => Unit): YPart = {
    val doc = YDocument(createPartBuilder(f)(0))
    _document = Some(doc)
    doc
  }

  private def createPartBuilder(f: Part[YPart] => Unit): ArrayBuffer[YNode] = {
    val builder = new ArrayBuffer[YNode]
    val partBuilder: Part[YPart] = new Part[YPart] {
      override def +=(scalar: Scalar): Unit = builder += mkNode(scalar)
      override def +=(element: YPart): Unit = element match {
        case node: YNode => builder += node
        case _           => // nothing
      }
      override def list(f: Part[YPart] => Unit): Option[YPart] = {
        val result = createSeqNode(f)
        builder += result
        Some(result)
      }
      override def obj(f: Entry[YPart] => Unit): Option[YPart] = {
        val result = createMapNode(f)
        builder += result
        Some(result)
      }
    }
    f(partBuilder)
    builder
  }

  private def mkNode(scalar: Scalar): YNode = scalar.t match {
    case Str   => scalar.value.toString
    case Bool  => scalar.value.asInstanceOf[Boolean]
    case Float => scalar.value.asInstanceOf[Double]
    case Int   => scalar.value.asInstanceOf[Long]
  }

  private def createMapNode(f: Entry[YPart] => Unit, inFlow: Boolean = false) = {
    val builder                                  = new ArrayBuffer[YPart]
    def addEntry(key: String, node: YNode): Unit = builder += YMapEntry(YNode(key), node)

    val b: Entry[YPart] = new Entry[YPart] {
      override def entry(key: String, value: Scalar): Unit          = addEntry(key, mkNode(value))
      override def entry(key: String, f: Part[YPart] => Unit): Unit = addEntry(key, createPartBuilder(f).result()(0))

    }
    f(b)
    YNode(YMap(builder.result, "", inFlow), YType.Map)
  }
  private def createSeqNode(f: Part[YPart] => Unit) = YNode(YSequence(createPartBuilder(f).result), YType.Seq)
}

object YDocumentBuilder {
  def apply(): YDocumentBuilder = new YDocumentBuilder()
  def apply(f: Part[YPart] => Unit): YPart = {
    val b = apply()
    b.doc(f)
    b.result
  }

}

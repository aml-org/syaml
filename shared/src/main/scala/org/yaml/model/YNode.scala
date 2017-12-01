package org.yaml.model

import org.yaml.model.YNode._

import scala.language.implicitConversions

/**
  * A Yaml Node, it has a Value plus Properties
  */
abstract class YNode(override val children: Parts) extends YNodeLike with YPart {

  def value: YValue
  def tag: YTag
  def anchor: Option[YAnchor] = None
  def tagType: YType          = tag.tagType

  override def equals(obj: scala.Any): Boolean = obj match {
    case _: YFail     => false
    case n: YNode     => this.tagType == n.tagType && this.value == n.value
    case n: YNodeLike => this == n.thisNode
    case _            => false
  }

  override def hashCode(): Int  = tagType.hashCode * 31 + value.hashCode
  override def toString: String = value.toString

  /** Create a reference (alias) to this node */
  def alias(): YNode = anchor match {
    case Some(a) => new YNode.Alias(a.name, this, noParts)
    case _       => throw new IllegalStateException("Node does not have an Anchor")
  }

  /** Create a new node with an anchor */
  def anchor(name: String): YNode = YNode(value, tagType, YAnchor(name))

  override def obj: YObj = YSuccess(this)

  override protected[model] def thisNode: YNode = this
}

object YNode {

  /** Create a direct Node Implementation */
  def apply(v: YValue, t: YTag, a: Option[YAnchor] = None, cs: Parts = null): YNode =
    new YNode(if (cs == null) Array(v) else cs) {
      override def value: YValue           = v
      override def tag: YTag               = t
      override def anchor: Option[YAnchor] = a
    }

  def apply(value: YValue, tt: YType, ref: YAnchor): YNode = YNode(value, tt.tag, Some(ref))
  def apply(value: YValue, tt: YType): YNode               = YNode(value, tt.tag)
  def apply(text: String): YNode                           = YNode(YScalar(text), YType.Str)
  def apply(int: Int): YNode                               = YNode(YScalar(int), YType.Int)
  def apply(long: Long): YNode                             = YNode(YScalar(long), YType.Int)
  def apply(bool: Boolean): YNode                          = YNode(YScalar(bool), YType.Bool)
  def apply(double: Double): YNode                         = YNode(YScalar(double), YType.Float)
  def apply(seq: YSequence): YNode                         = YNode(seq, YType.Seq)
  def apply(map: YMap): YNode                              = YNode(map, YType.Map)

  val Null = YNode(YScalar.Null, YType.Null)

  // Implicit conversions

  implicit def toString(node: YNode): String     = node.as[String]
  implicit def toInt(node: YNode): Int           = node.as[Int]
  implicit def toLong(node: YNode): Long         = node.as[Long]
  implicit def toBoolean(node: YNode): Boolean   = node.as[Boolean]
  implicit def toDouble(node: YNode): Double     = node.as[Double]
  implicit def fromString(str: String): YNode    = YNode(str)
  implicit def fromInt(int: Int): YNode          = YNode(int)
  implicit def fromLong(long: Long): YNode       = YNode(long)
  implicit def fromBool(bool: Boolean): YNode    = YNode(bool)
  implicit def fromDouble(double: Double): YNode = YNode(double)
  implicit def fromSeq(seq: YSequence): YNode    = YNode(seq)
  implicit def fromMap(map: YMap): YNode         = YNode(map)

  /** An Include Node */
  def include(uri: String): MutRef = {
      val v = YScalar(uri)
      val t = YType.Include.tag
      new MutRef(v, t, Array(t, v))
  }

  private type Parts = IndexedSeq[YPart]
  private final val noParts: Parts = IndexedSeq.empty

  /**
    * A Yaml Node Reference, methods are redirected to the target node
    */
  abstract class Ref(cs: Parts) extends YNode(cs)

  /** A Mutable Node reference */
  final class MutRef(val origValue: YValue, val origTag: YTag, val cs: Parts) extends Ref(cs) {
    var target: Option[YNode]  = None
    override def value: YValue = target.map(_.value).getOrElse(origValue)
    override def tag: YTag     = target.map(_.tag).getOrElse(origTag)
  }

  /** An Alias Node */
  final class Alias(val name: String, val target: YNode, cs: Parts) extends Ref(cs) {
    override def value: YValue    = target.value
    override def tag: YTag        = target.tag
    override def toString: String = "*" + name
  }

}

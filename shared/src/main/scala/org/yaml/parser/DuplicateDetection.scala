package org.yaml.parser

import org.yaml.model._

import scala.collection.mutable

trait DuplicateDetection {

  def duplicates(parts: Array[YPart])(implicit errorHandler: ParseErrorHandler): Unit = {
    val keys = mutable.Set[String]()
    for (part <- parts) part match {
      case entry: YMapEntry =>
        entry.key.value match {
          case s: YScalar =>
            val key = s.text
            if (!keys.add(key)) errorHandler.handle(entry.key.location, DuplicateKeyException(key))
          case _ =>
        }
      case _ =>
    }
  }
}

package org.yaml.model

import org.mulesoft.common.client.lexical.SourceLocation

trait DefaultJsonErrorHandler extends ParseErrorHandler {

  protected val errorHandler: ParseErrorHandler = ParseErrorHandler.parseErrorHandler

  override def handle(location: SourceLocation, e: SyamlException): Unit = e match {
    case _: DuplicateKeyException => onIgnoredException(location, e)
    case _                        => errorHandler.handle(location, e)
  }

  protected def onIgnoredException(location: SourceLocation, e: SyamlException): Unit = Unit
}

object DefaultJsonErrorHandler {
  def apply(): DefaultJsonErrorHandler = new DefaultJsonErrorHandler {}
}

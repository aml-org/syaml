package org.yaml.model
import org.mulesoft.lexer.SourceLocation

private class DefaultJsonErrorHandler extends ParseErrorHandler {

  private lazy val errorHandler: ParseErrorHandler = ParseErrorHandler.parseErrorHandler

  override def handle(location: SourceLocation, e: SyamlException): Unit = e match {
    case _: DuplicateKeyException => // ignore
    case _ => errorHandler.handle(location, e)
  }
}

object DefaultJsonErrorHandler {
  def apply(): DefaultJsonErrorHandler = new DefaultJsonErrorHandler()
}
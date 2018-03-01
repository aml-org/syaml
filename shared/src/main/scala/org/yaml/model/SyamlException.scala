package org.yaml.model

case class ParseException(tag: YType, text: String, cause: Exception = null)
  extends SyamlException(s"Cannot parse '$text' with tag '$tag'", cause)

case class LexerException(text: String, cause: Exception = null)
  extends SyamlException(s"Error node '$text'", cause)

abstract class SyamlException(message: String, cause: Exception)
  extends RuntimeException(message, cause) {}

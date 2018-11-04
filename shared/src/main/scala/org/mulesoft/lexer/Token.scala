package org.mulesoft.lexer

import org.mulesoft.common.core._

/**
  * The Token Trait
  */
abstract class Token(val name: String, val abbreviation: String) {
    override def toString: String = name
}

/**
  * The Token data
  */
case class TokenData[T <: Token](token: T, range: InputRange, start: Int = 0, end: Int = 0) {
  def rangeTo(to: TokenData[T]): InputRange =
    InputRange(range.lineFrom, range.columnFrom, to.range.lineTo, to.range.columnTo)
}

/**
  * A Token that contains the associated text.
  * So it can be used to into the AST to serialize the input without keeping the original source
  */
case class AstToken(tokenType: Token, text: String, range: InputRange, parsingError :Boolean = false) {
    override def toString: String =  s"$tokenType '${text.encode}'"
}

package org.mulesoft.lexer

import org.mulesoft.lexer.LexerInput.{EofChar, Mark}

import scala.collection.mutable.ArrayBuffer

abstract class BaseLexer[T <: Token](var input: LexerInput) extends Lexer[T] {

  type TD = TokenData[T]
  val offsetPosition: (Int, Int)
  private val tokenQueue               = new Queue[TD]
  private var mark                     = position
  val sourceName: String               = input.sourceName
  private var _tokenData: TokenData[T] = _

  /** initialize the current _tokenData (may be invoking advance) */
  initialize()

  private def position = (input.position._1 + offsetPosition._1, input.position._2 + offsetPosition._2, input.position._3)
  /** Check if there are emitted tokens */
  def nonTokenEmitted: Boolean = tokenQueue.isEmpty

  /** Init must initialize the current _tokenData (may be invoking advance) */
  protected def initialize()

  /** get the current token in the input stream.  */
  override def token: T = _tokenData.token

  /** All the token data.  */
  override def tokenData: TD = _tokenData

  /** Get the specified Token Char Sequence.  */
  def tokenText(td: TD): CharSequence = input.subSequence(td.start, td.end)

  /** Get the current Token Char Sequence.  */
  override def tokenText: CharSequence = tokenText(_tokenData)

  /** Emit a Token */
  @failfast def emit(token: T): Boolean = {
    val newMark = position
    tokenQueue += TokenData(token, InputRange(mark._1, mark._2, newMark._1, newMark._2), mark._3, newMark._3)
    mark = newMark
    true
  }

  /** Emit 2 Tokens */
  @failfast def emit(t1: T, t2: T): Boolean = {
    emit(t1)
    emit(t2)
  }

  /** Emit 2 Tokens with same mark */
  @failfast protected def emitForMark(t1: T, t2: T): Boolean = {
    val initialMark = mark
    emit(t1)
    mark = initialMark
    emit(t2)
  }

  protected def reset(): Unit = mark = position

  protected def findToken(chr: Int):Unit = {}

  /** Advance the lexer to the next token.  */
  override final def advance(): Unit = {
    while (nonTokenEmitted) {
      if (currentChar != EofChar) {
          val p = input.offset
          findToken(currentChar)
          if (p == input.offset) {
              advance()
          }

      }
      else processPending()
    }
    _tokenData = tokenQueue.dequeue
  }

  protected final def currentChar: Int = input.current

  final def lookAhead(n: Int): Int = input.lookAhead(n)

  final def consume(): Unit = input.consume()

  protected final def consume(n: Int): Unit             = input.consume(n)
  protected def consumeWhile(p: (Int => Boolean)): Unit = input.consumeWhile(p)

  /** Compare with the specified char and consume if they are equal */
  protected def consume(c: Char): Boolean =
    if (currentChar != c) false
    else {
      consume()
      true
    }

  /** Compare with the specified String and consume if all characters are equal */
  final def consume(str: String): Boolean = {
    val l = check(str)
    if (l == 0) false
    else {
      consume(l)
      true
    }
  }

  /** Compare with the specified String and return 0 or the length of the string if all characters are equal */
  final def check(str: String): Int = {
    val len = str.length
    if (len > 0 && (0 until len).forall(i => str(i) == lookAhead(i))) len else 0
  }

  final def consumeAndEmit(token: T): Boolean = {
    consume()
    emit(token)
  }
  final def consumeAndEmit(t1: T, t2: T): Boolean = {
    consume()
    emit(t1)
    emit(t2)
  }
  final def consumeAndEmit(n: Int, token: T): Boolean =
    if (n <= 0) true
    else {
      consume(n)
      emit(token)
    }
  final def matches(p: => Boolean): Boolean = {
    val s      = saveState
    val result = p
    if (!result) restoreState(s)
    result
  }

  final def zeroOrMore(p: => Boolean): Boolean = {
    var s = saveState
    while (nonEof && p) s = saveState
    restoreState(s)
    true
  }
  final def oneOrMore(p: => Boolean): Boolean = {
    var s      = saveState
    val result = p
    if (result) {
      do s = saveState while (nonEof && p)
    }
    restoreState(s)
    result
  }

  /** We're not at the Eof */
  def nonEof: Boolean = input.nonEof

  final def optional(p: Boolean): Boolean = true

  @inline final def beginOfLine: Boolean = input.column == 0

  /**
    * Process all pending tokens. Trivial implementation just emit the EofToken
    * More complex ones can continue returning pending tokens until they emit the EofToken
    */
  protected def processPending(): Unit

  def restoreState(s: (Int, (Int, Int, Int), Mark)): Unit = {
    tokenQueue.reduceTo(s._1)
    mark = s._2
    input.reset(s._3)
  }

  def saveState: (Int, (Int, Int, Int), Mark) =
    (tokenQueue.size, mark, input.createMark())
}
class Queue[T] {
    private val buffer = ArrayBuffer.empty[T]
    private var head = 0
    private var tail = 0
    def size:Int = tail - head
    def +=(t : T): this.type = {
        if (buffer.size <= tail) buffer.append(t)
        else buffer(tail) = t
        tail += 1
        this
    }
    def reduceTo(newSize: Int): Unit = tail = head + newSize
    def dequeue: T = {
        head += 1
        buffer(head-1)
    }
    def isEmpty: Boolean = tail <= head
}


object Position{
  val ZERO: (Int, Int) = (0,0)
}
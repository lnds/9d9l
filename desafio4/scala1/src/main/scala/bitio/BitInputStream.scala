package bitio

import java.io.{IOException, InputStream}

/**
  * Input bits from a stream
  * Created by ediaz on 7/15/17.
  */
class BitInputStream(val input:InputStream) extends BitStream(input) {

  private val EOF = -1

  fillBuffer()

  private[this] def fillBuffer() : Unit = {
    try {
      buffer = input.read()
      bitsInBuffer = 8
    } catch {
      case e:IOException =>
        buffer = EOF
        bitsInBuffer = -1
    }
  }

  def isEmpty() : Boolean = buffer == EOF

  def readChar() : Char = {
    if (isEmpty()) {
      throw new NoSuchElementException("reading from empty input stream")
    }

    if (bitsInBuffer == 8) {
      val x = buffer
      fillBuffer()
      return (x & 0xFF).toChar
    }

    var x = buffer
    x = x << (8 - bitsInBuffer)
    val temp = bitsInBuffer
    fillBuffer()
    if (isEmpty()) { throw new NoSuchElementException("Reading from empty input stream") }
    bitsInBuffer = temp
    x |= (buffer >>> bitsInBuffer)
    (x & 0xFF).toChar
  }

  def readInt() : Int = {
    var x = 0
    (0 to 3).foreach { i =>
      val c = readChar()
      x = (x << 8) | c.toInt
    }
    x
  }

  def readBoolean() : Boolean = {
    if (isEmpty()) throw new NoSuchElementException("Reading from empty input stream")
    bitsInBuffer -= 1
    val bit = ((buffer >> bitsInBuffer) & 1) == 1
    if (bitsInBuffer == 0) { fillBuffer() }
    bit
  }
}

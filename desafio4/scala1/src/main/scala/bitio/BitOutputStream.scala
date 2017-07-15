package bitio

import java.io.OutputStream

/**
  * Output bits on a stream
  * Created by ediaz on 7/15/17.
  */
class BitOutputStream(val output: OutputStream) extends BitStream(output) {

  private[this] def writeBit(bit: Int) : Unit = {
    if (bit != 0 && bit != 1) {
      throw new IllegalArgumentException("Argument must be 0 or 1. Received: "+bit)
    }

    buffer = (buffer << 1) | bit
    bitsInBuffer += 1
    if (bitsInBuffer == 8) {
      clearBuffer()
    }
  }

  private[this] def clearBuffer(): Unit = {
    if (bitsInBuffer == 0) {
      return
    }
    if (bitsInBuffer > 0) {
      buffer = buffer << (8 - bitsInBuffer)
    }
    output.write(buffer)
    buffer = 0
    bitsInBuffer = 0
  }

  private[this] def writeByte(byte: Int) : Unit = {
    if (byte < 0 || byte >= 256)
      throw new IllegalArgumentException("Argument must be in range 0..255. Received: "+byte)

    (0 to 7).foreach { i =>
      val bit = (byte >>> (8 - i - 1)) & 1
      writeBit(bit)
    }
  }

  def flush() : Unit = {
    clearBuffer()
    output.flush()
  }

  override def close(): Unit = {
    flush()
    super.close()
  }

  def write(b: Boolean) : Unit = {
    writeBit(if (b) 1 else 0)
  }

  def write(i: Int): Unit = {
    writeByte((i >>> 24) & 0xFF)
    writeByte((i >>> 16) & 0xFF)
    writeByte((i >>>  8) & 0xFF)
    writeByte((i >>>  0) & 0xFF)
  }

  def write(c: Char) : Unit = {
    writeByte(c & 0xFF)
  }
}

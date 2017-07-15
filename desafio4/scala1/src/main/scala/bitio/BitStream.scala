package bitio

import java.io.Closeable

/**
  * Created by ediaz on 7/15/17.
  */
abstract class BitStream(val stream:Closeable) {

  protected var buffer : Int = 0
  protected var bitsInBuffer : Int = 0

  def close(): Unit = {
    stream.close()
  }
}

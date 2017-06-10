package huffman

import java.io.IOException
import java.io.InputStream
import java.util.NoSuchElementException

/**
 * Input bits from a stream
 * Created by ediaz on 6/3/17.
 */
class BitInputStream(val input : InputStream) : BitStream(input)  {

    private val EOF = -1

    init {
        fillBuffer()
    }

    private fun fillBuffer() {
        try {
            buffer = input.read()
            bitsInBuffer = 8
        } catch (e:IOException) {
            buffer = EOF
            bitsInBuffer = -1
        }
    }

    fun isEmpty() = buffer == EOF

    fun readChar(): Char {
        if (isEmpty()) throw NoSuchElementException("Reading from empty input stream")

        // special case when aligned byte
        if (bitsInBuffer == 8) {
            val x = buffer
            fillBuffer()
            return (x and 0xff).toChar()
        }

        // combine last n bits of current buffer with first 8-n bits of new buffer
        var x = buffer
        x = x shl (8 - bitsInBuffer)
        val temp = bitsInBuffer
        fillBuffer()
        if (isEmpty()) throw NoSuchElementException("Reading from empty input stream")
        bitsInBuffer = temp
        x = x or (buffer ushr bitsInBuffer)
        return (x and 0xff).toChar()
    }


    fun readInt(): Int {
        var x = 0
        (0..3).forEach { i ->
            val c = readChar()
            x = (x shl 8) or c.toInt()
        }
        return x
    }

    fun readBoolean(): Boolean {
        if (isEmpty()) throw NoSuchElementException("Reading from empty input stream")
        bitsInBuffer--
        val bit = ((buffer shr bitsInBuffer) and 1) == 1
        if (bitsInBuffer == 0) fillBuffer()
        return bit
    }
}
package huffman

import java.io.OutputStream

/**
 * Output bits on a stream
 * Created by ediaz on 5/21/17.
 */
class BitOutputStream(val output : OutputStream) : BitStream(output) {


    private fun writeBit(bit: Int) {
        if (bit != 0 && bit != 1)
            throw IllegalArgumentException("Argument must be '0' or '1'. Received: "+bit)

        buffer = (buffer shl 1) or bit
        bitsInBuffer++
        if (bitsInBuffer == 8) {
            clearBuffer()
        }
    }

    private fun writeByte(byte:Int) {
        if (byte < 0 || byte >= 256)
            throw IllegalArgumentException("Argument must be in range 0..255. Received: "+byte)

        (0..7).forEach { i ->
            val bit = ((byte ushr (8 - i -1)) and 1)
            writeBit(bit)
        }
    }

    private fun clearBuffer() {
        if (bitsInBuffer == 0)
            return
        if (bitsInBuffer > 0)
            buffer = buffer shl (8 - bitsInBuffer)

        output.write(buffer)
        buffer = 0
        bitsInBuffer = 0
    }

    fun flush() {
        clearBuffer()
        output.flush()
    }

    override fun close() {
        flush()
        super.close()
    }

    fun write(b: Boolean) {
        writeBit(if (b) 1 else 0)
    }

    fun write(i: Int) {
        writeByte((i ushr 24) and 0xFF)
        writeByte((i ushr 16) and 0xFF)
        writeByte((i ushr  8) and 0xFF)
        writeByte((i ushr  0) and 0xFF)
    }

    fun write(c: Char) {
        writeByte(c.toInt() and 0xFF)
    }

}
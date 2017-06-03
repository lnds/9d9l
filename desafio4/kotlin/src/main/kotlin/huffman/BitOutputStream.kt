package huffman

import java.io.OutputStream

/**
 * Output bits on a stream
 * Created by ediaz on 5/21/17.
 */
class BitOutputStream(val output : OutputStream) {

    var buffer : Int = 0
    var bitsInBuffer : Int = 0


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
        assert(byte >= 0 && byte <= 256)
        for (i in 0..8){
            val bit = ((byte ushr (8 - i -1)) and 1)
            writeBit(bit)
        }
    }

    private fun clearBuffer() {
        output.write(buffer)
        buffer = 0
        bitsInBuffer = 0
    }


    fun flush() {
        clearBuffer()
        output.flush()
    }

    fun close() {
        flush()
        output.close()
    }

    fun write(b: Boolean) {
        writeBit(if (b) 1 else 0)
    }

    fun write(b: Byte) {
        writeByte(b.toInt() and 0xFF)
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
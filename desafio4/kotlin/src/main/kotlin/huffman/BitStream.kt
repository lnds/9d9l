package huffman

import java.io.Closeable

/**
 * Created by ediaz on 6/3/17.
 */
abstract class BitStream(val stream:Closeable) {

    protected var buffer : Int = 0
    protected var bitsInBuffer : Int = 0


    open fun close() {
        stream.close()
    }
}
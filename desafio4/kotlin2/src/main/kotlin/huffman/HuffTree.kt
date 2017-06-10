package huffman

/**
 * Build HuffmanTree from frequency table of symbols
 * Created by ediaz on 5/21/17.
 */

fun Char.index() = this.toInt() and 0xFFFF
fun Byte.index() = this.toInt() and 0xFFFF

abstract class HuffTree(val frequency : Int) {

    abstract fun writeTo(output: BitOutputStream)
    abstract fun dumpCodes(codes: Array<String>, prefix : StringBuffer)
    abstract fun readChar(input: BitInputStream) : Char
}

class HuffLeaf(frequency: Int, private val symbol: Char) : HuffTree(frequency) {

    fun symbolIndex(): Int = symbol.index()

    override fun writeTo(output: BitOutputStream){
        output.write(true)
        output.write(symbol)
    }

    override fun dumpCodes(codes: Array<String>, prefix : StringBuffer) {
        codes[symbolIndex()] = prefix.toString()
    }

    override fun readChar(input: BitInputStream) : Char {
        return this.symbol
    }
}

class HuffNode(private val left: HuffTree, private val right: HuffTree) : HuffTree(left.frequency + right.frequency) {

    override fun writeTo(output: BitOutputStream){
        output.write(false)
        left.writeTo(output)
        right.writeTo(output)
    }

    override fun dumpCodes(codes: Array<String>, prefix : StringBuffer) {
        prefix.append('0')
        left.dumpCodes(codes, prefix)
        prefix.deleteCharAt(prefix.lastIndex)

        prefix.append('1')
        right.dumpCodes(codes, prefix)
        prefix.deleteCharAt(prefix.lastIndex)
    }

    override fun readChar(input: BitInputStream) : Char {
        val bit = input.readBoolean()
        return if (bit) right.readChar(input) else left.readChar(input)
    }
}

val maxSymbols = 256

class HuffHeap {

    var heap = arrayOfNulls<HuffTree>(maxSymbols+1)
    var last = 0

    fun insert(tree:HuffTree) {
        if (full()) {
            throw ArrayIndexOutOfBoundsException()
        }

        last++
        heap[last] = tree
        var j = last
        while (j > 1){
            if (heap[j]!!.frequency < heap[j/2]!!.frequency) {
                val tmp = heap[j]!!
                heap[j] = heap[j / 2]!!
                heap[j / 2] = tmp
            }
            j /= 2
        }
    }

    fun full() = last == maxSymbols

    fun empty() = last == 0

    fun size() = last

    fun extract() : HuffTree {
        if (empty()) {
            throw ArrayIndexOutOfBoundsException()
        }

        val min = heap[1]!!
        heap[1] = heap[last]
        last--
        var j = 1
        while (2*j <= last) {
            var k = 2*j
            if (k+1 <= last && heap[k+1]!!.frequency < heap[k]!!.frequency) {
                k++
            }
            if (heap[j]!!.frequency < heap[k]!!.frequency) {
                break
            }
            val tmp = heap[j]!!
            heap[j] = heap[k]
            heap[k]  = tmp
            j = k
        }
        return min
    }
}

fun buildTree(freqs : IntArray) : HuffTree {
    val heap = HuffHeap()
    freqs.forEachIndexed { sym, freq ->
        if (freq > 0) {
            heap.insert(HuffLeaf(freq, sym.toChar()))
        }
    }

    while (heap.size() > 1) {
        val a = heap.extract()
        val b = heap.extract()
        heap.insert(HuffNode(a, b))
    }
    return heap.extract()
}

fun readTree(input : BitInputStream) : HuffTree =
    if (input.readBoolean())
        HuffLeaf(-1, input.readChar())
    else
        HuffNode(readTree(input), readTree(input))

fun buildCodes(tree: HuffTree) : Array<String> {
    val prefix = StringBuffer()
    val codes = Array(maxSymbols, {""})
    tree.dumpCodes(codes, prefix)
    return codes
}

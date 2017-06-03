package huffman

/**
 * Build HuffmanTree from frequency table of symbols
 * Created by ediaz on 5/21/17.
 */
abstract class HuffTree(val frequency : Int) {
}

class HuffLeaf(frequency: Int, val symbol: Char) : HuffTree(frequency) {

    fun symbolIndex(): Int = symbol.toInt() and 0xFFFF

}

val maxSymbols = 256

class HuffNode(val left: HuffTree, val right: HuffTree) : HuffTree(left.frequency+right.frequency)

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
    val trees = HuffHeap()
    freqs.forEachIndexed { sym, freq ->
        if (freq > 0) {
            trees.insert(HuffLeaf(freq, sym.toChar()))
        }
    }

    while (trees.size() > 1) {
        val a = trees.extract()
        val b = trees.extract()
        trees.insert(HuffNode(a, b))
    }
    return trees.extract()
}

fun buildCodes(tree: HuffTree) : Array<String> {
    val prefix = StringBuffer()
    val codes = Array<String>(maxSymbols, {""})
    buildCodes(tree, codes, prefix)
    return codes
}


fun buildCodes(tree : HuffTree, codes: Array<String>, prefix : StringBuffer) {
    when (tree) {
        is HuffLeaf -> codes[tree.symbolIndex()] = prefix.toString()

        is HuffNode -> {
            prefix.append('0')
            buildCodes(tree.left, codes, prefix)
            prefix.deleteCharAt(prefix.lastIndex)

            prefix.append('1')
            buildCodes(tree.right, codes, prefix)
            prefix.deleteCharAt(prefix.lastIndex)
        }
    }
}
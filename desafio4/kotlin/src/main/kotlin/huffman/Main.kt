package huffman

import java.io.*

/**
 * Main entry point
 */
fun main(args: Array<String>) {
    if (args.size != 3)
        usage()
    else
        process(args)
}

fun  process(args: Array<String>) {
    val opt = args[0]
    when (opt) {
        "-c" -> compress(args[1], args[2])
        "-d" -> decompress(args[1], args[2])
        else -> usage()
    }
}

fun usage() {
    println("Uso: huffman [-c|-d] archivo_entrada archivo_salida")
}

fun compress(inputFile: String, outputFile: String) {
    // read file as an array of bytes
    val bytes = File(inputFile).readBytes()
    val freqs = calcFrequencies(bytes)
    val huffTree = buildTree(freqs)
    val codes = buildCodes(huffTree)
    val output = BitOutputStream(BufferedOutputStream(FileOutputStream(outputFile)))

    writeTree(huffTree, output)
    println("comprime, size="+bytes.size)
    output.write(bytes.size)
    bytes.forEach { symbol ->
        val code = codes[symbol.toInt() and 0xFFFF]
        code.forEach { bit -> output.write(bit == '1') }
    }
    output.close()
}



fun  decompress(inputFile: String, outputFile: String) {
    val input = BitInputStream(BufferedInputStream(FileInputStream(inputFile)))
    val output = BitOutputStream(BufferedOutputStream(FileOutputStream(outputFile)))

    val huffTree = readTree(input)
    val length = input.readInt()
    println("descomprime, size="+length)

    for (i in 0..length-1) {
        var node = huffTree
        while (node is HuffNode) {
            val bit = input.readBoolean()
            node = if (bit) node.right else node.left
        }
        if (node is HuffLeaf)
            output.write(node.symbol)
    }
    output.close()
}


fun calcFrequencies(bytes:ByteArray) : IntArray {
    // calc frequencies
    val freqs = IntArray(Symbols.maxSymbols+1)
    bytes.forEach { symbol -> freqs[symbol.toInt() and 0xFFFF]++ }
    return freqs
}

fun writeTree(tree:HuffTree, out:BitOutputStream) {
    when (tree) {
        is HuffLeaf -> {
            out.write(true)
            out.write(tree.symbol)
        }

        is HuffNode -> {
            out.write(false)
            writeTree(tree.left, out)
            writeTree(tree.right, out)
        }
    }
}

fun readTree(input : BitInputStream) : HuffTree = if (input.readBoolean())
                                                      HuffLeaf(-1, input.readChar())
                                                  else
                                                      HuffNode(readTree(input), readTree(input))

package huffman

import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream

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
    val out = BitOutputStream(BufferedOutputStream(FileOutputStream(outputFile)))

    writeTree(huffTree, out)
    out.write(bytes.size)
    bytes.forEach { symbol ->
        val code = codes[symbol.toInt() and 0xFFFF]
        code.forEach { bit -> out.write(bit == '1') }
    }
    out.close()
}



fun  decompress(inputFile: String, outputFile: String) {
    println("decompress ${inputFile} ${outputFile}")
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



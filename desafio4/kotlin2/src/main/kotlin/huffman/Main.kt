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

fun usage() = println("Uso: huffman [-c|-d] archivo_entrada archivo_salida")

private fun compress(inputFile: String, outputFile: String) {
    // read file as an array of bytes
    val bytes = File(inputFile).readBytes()
    val freqs = calcFrequencies(bytes)
    val huffTree = buildTree(freqs)
    val codes = buildCodes(huffTree)
    val output = BitOutputStream(BufferedOutputStream(FileOutputStream(outputFile)))

    huffTree.writeTo(output)
    output.write(bytes.size)
    bytes.forEach { symbol ->
        val code = codes[symbol.index()]
        code.forEach { bit -> output.write(bit == '1') }
    }
    output.close()
}

private fun  decompress(inputFile: String, outputFile: String) {
    val input = BitInputStream(BufferedInputStream(FileInputStream(inputFile)))
    val output = BitOutputStream(BufferedOutputStream(FileOutputStream(outputFile)))

    val huffTree = readTree(input)
    val length = input.readInt()

    (0..length-1).forEach { output.write(huffTree.readChar(input)) }
    output.close()
}

fun calcFrequencies(bytes:ByteArray) : IntArray {
    // calc frequencies
    val freqs = IntArray(maxSymbols)
    bytes.forEach { symbol -> freqs[symbol.index()]++ }
    return freqs
}


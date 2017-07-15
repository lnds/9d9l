import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}

import bitio.{BitInputStream, BitOutputStream}
import huffman.HuffTree

object Main {

  def main(args: Array[String]) : Unit = {
    if (args.length != 3)
      usage()
    else
      process(args)
  }

  def usage() : Unit = {
    println("Uso: huffman [-c|-d] archivo_entrada archivo_salida")
  }

  def process(args: Array[String]) : Unit =
    args(0) match {
      case "-c" => compress(args(1), args(2))
      case "-d" => decompress(args(1), args(2))
      case _ => usage()
    }

  def compress(inputFile: String, outputFile: String) : Unit = {
    val bytes = Files.readAllBytes(Paths.get(inputFile))
    val freqs = calcFrequencies(bytes)
    val huffTree = HuffTree.build(freqs)
    val codes = HuffTree.buildCodes(huffTree)
    val output = new BitOutputStream(new BufferedOutputStream(new FileOutputStream(outputFile)))
    huffTree.writeTo(output)
    output.write(bytes.size)
    bytes.foreach { symbol =>
      val code = codes(symbol & 0xFF)
      code.foreach { bit => output.write(bit == '1') }
    }
    output.close()
  }

  def decompress(inputFile: String, outputFile: String) : Unit = {
    val input = new BitInputStream(new BufferedInputStream(new FileInputStream(inputFile)))
    val output = new BitOutputStream(new BufferedOutputStream(new FileOutputStream(outputFile)))

    val huffTree = HuffTree.read(input)
    val length = input.readInt()

    (0 until length).foreach { _ => output.write(huffTree.readChar(input))}
    output.close()
  }

  private[this] def calcFrequencies(bytes: Array[Byte]) : Array[Int] = {
    val freqs = Array.fill[Int](HuffTree.maxSymbols)(0)
    bytes.foreach(symbol => freqs(symbol & 0xFF) += 1)
    freqs
  }

}
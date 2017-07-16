package huffman

import bitio.{BitInputStream, BitOutputStream}

import scala.util.control.Breaks._

/**
  * Build HuffmanTree from frequency table of symbols
  * Created by ediaz on 7/15/17.
  */
abstract class HuffTree(val frequency: Int) {

  def dumpCodes(codes : Array[String], prefix: StringBuffer)
  def writeTo(output: BitOutputStream)
  def readChar(input: BitInputStream) : Char

}

case class HuffLeaf(override val frequency: Int,  symbol: Char) extends HuffTree(frequency = frequency) {


  override def dumpCodes(codes: Array[String], prefix: StringBuffer): Unit = {
    codes(symbol & 0xFF) = prefix.toString
  }

  override def writeTo(output: BitOutputStream): Unit = {
    output.write(true)
    output.write(symbol)
  }

  override def readChar(input: BitInputStream): Char = {
    symbol
  }

}

case class HuffNode(left:HuffTree, right: HuffTree) extends HuffTree(left.frequency+right.frequency) {

  override def dumpCodes(codes: Array[String], prefix: StringBuffer): Unit = {
    prefix.append('0')
    left.dumpCodes(codes, prefix)
    prefix.deleteCharAt(prefix.length() - 1)

    prefix.append('1')
    right.dumpCodes(codes, prefix)
    prefix.deleteCharAt(prefix.length() - 1)
  }

  override def writeTo(output: BitOutputStream): Unit = {
    output.write(false)
    left.writeTo(output)
    right.writeTo(output)
  }

  override def readChar(input: BitInputStream): Char = {
    val bit = input.readBoolean()
    if (bit) right.readChar(input) else left.readChar(input)
  }
}

object HuffTree {

  val maxSymbols = 256


  def build(freqs: List[(Char, Int)]): HuffTree = {

    val leaves = freqs
      .map { case (sym, freq) => HuffLeaf(freq, sym) }.distinct
      .sortWith((l1: HuffLeaf, l2: HuffLeaf) => l1.frequency < l2.frequency)
    until(singleton, combine)(leaves).head
  }

  def singleton(trees: List[HuffTree]) : Boolean = trees.length == 1

  def combine(trees: List[HuffTree]) : List[HuffTree] =
    trees match {
      case Nil => Nil
      case left :: right :: tail => (HuffNode(left, right) :: tail).sortWith((l1,l2) => l1.frequency < l2.frequency)
    }

  def until[A](singleton: A => Boolean, combine: A => A)(data: A) : A =
    if (singleton(data)) data else until(singleton, combine)(combine(data))

  def read(input: BitInputStream): HuffTree =
    if (input.readBoolean())
      HuffLeaf(-1, input.readChar())
    else
      HuffNode(read(input), read(input))


  def buildCodes(tree: HuffTree): Array[String] = {
    val prefix = new StringBuffer()
    val codes = Array.fill[String](maxSymbols)("")
    tree.dumpCodes(codes, prefix)
    codes
  }

}
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

class HuffLeaf(override val frequency: Int,  symbol: Char) extends HuffTree(frequency = frequency) {

  val symbolIndex : Int = symbol.toInt & 0xFFFF

  override def dumpCodes(codes: Array[String], prefix: StringBuffer): Unit = {
    codes(symbolIndex) = prefix.toString
  }

  override def writeTo(output: BitOutputStream): Unit = {
    output.write(true)
    output.write(symbol)
  }

  override def readChar(input: BitInputStream): Char = {
    symbol
  }

}

class HuffNode(left:HuffTree, right: HuffTree) extends HuffTree(left.frequency+right.frequency) {

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

class HuffHeap(maxSymbols:Int) {


  private[this] var heap = Array.fill[HuffTree](maxSymbols+1)(null)

  private[this] var last = 0

  def insert(tree:HuffTree) : Unit = {
    if (full()) {
      throw new ArrayIndexOutOfBoundsException()
    }
    last += 1
    heap(last) = tree
    var j = last
    while (j > 1) {
      if (heap(j).frequency < heap(j/2).frequency) {
        val tmp = heap(j)
        heap(j) = heap(j/2)
        heap(j/2) = tmp
      }
      j /= 2
    }
  }

  def full(): Boolean = last == maxSymbols

  def empty(): Boolean = last == 0

  def size(): Int = last

  def extract() : HuffTree = {
    if (empty()) {
      throw new ArrayIndexOutOfBoundsException()
    }
    val min = heap(1)
    heap(1) = heap(last)
    last -= 1
    var j = 1
    breakable {
      while (2 * j <= last) {
        var k = 2 * j
        if (k + 1 <= last && heap(k + 1).frequency < heap(k).frequency) {
          k += 1
        }
        if (heap(j).frequency < heap(k).frequency) {
          break
        }
        val tmp = heap(j)
        heap(j) = heap(k)
        heap(k) = tmp
        j = k
      }
    }
    min
  }

}

object HuffTree {

  val maxSymbols = 256


  def build(freqs: Array[Int]): HuffTree = {
    val heap = new HuffHeap(maxSymbols)
    freqs.zipWithIndex.foreach {
      case (freq, sym) =>
        if (freq > 0) {
          heap.insert(new HuffLeaf(freq, sym.toChar))
        }
    }
    while (heap.size() > 1) {
      val a = heap.extract()
      val b = heap.extract()
      heap.insert(new HuffNode(a, b))
    }
    heap.extract()
  }

  def read(input: BitInputStream): HuffTree =
    if (input.readBoolean())
      new HuffLeaf(-1, input.readChar())
    else {
      val l = read(input)
      val r = read(input)
      new HuffNode(l, r)
    }

  def buildCodes(tree: HuffTree): Array[String] = {
    val prefix = new StringBuffer()
    val codes = Array.fill[String](maxSymbols)("")
    tree.dumpCodes(codes, prefix)
    codes
  }

}
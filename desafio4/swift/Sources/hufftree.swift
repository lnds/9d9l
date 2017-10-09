import Foundation

let MAX_SYMBOLS = 256

protocol Tree {
	
	func freq() -> UInt

	func writeTo(writer: BitOutputStream)

	func dumpCodes(codes: inout [String], prefix: String)

	func readChar(reader: BitInputStream) -> UInt8
}


class Leaf : Tree {
	
	var frecuency : UInt
	var symbol : UInt8

	init(frq:UInt, sym:UInt8) {
		frecuency = frq
		symbol = sym
	}

	func freq() -> UInt {
		return frecuency
	}

	func writeTo(writer: BitOutputStream) {
		writer.writeBit(bit: 1)
		writer.writeByte(byte: UInt16(symbol))
	}

	func dumpCodes(codes: inout [String], prefix: String) {
		codes[Int(symbol)] = prefix
	}

	func readChar(reader: BitInputStream) -> UInt8 {
		return symbol
	}
}


class Node : Tree {
	var left: Tree
	var right: Tree

	init( left: Tree, right: Tree) {
		self.left = left
		self.right = right
	}	

	func freq() -> UInt {
		return left.freq() + right.freq()
	}

	func writeTo(writer: BitOutputStream) {
		writer.writeBit(bit: 0)
		left.writeTo(writer: writer)
		right.writeTo(writer: writer)
	}

	func dumpCodes(codes: inout [String], prefix: String) {
		left.dumpCodes(codes: &codes, prefix: prefix+"0")
		right.dumpCodes(codes: &codes, prefix: prefix+"1")
	}

	func readChar(reader: BitInputStream) -> UInt8 {
		if reader.readBool() {
			return right.readChar(reader: reader)
		} else {
			return left.readChar(reader: reader)
		}
	}
}

class HuffTree {


	var tree: Tree? = nil 
	var codes: [String] = [String]()

	init(buildFrom: BitInputStream) {
		let freqs = calcFrecuencies(source: buildFrom)
		let heap = Heap(size: MAX_SYMBOLS)

		for (s, f) in freqs.enumerated() { 
			if f > 0 {
				heap.insert(elem: Leaf(frq: UInt(f), sym: UInt8(s)))
			}
		}

		while heap.size() > 1 {
			let l = heap.extract()
			let r = heap.extract()
			heap.insert(elem: Node(left: l!, right: r!))
		}
		tree = heap.extract()
		codes = buildCodes(tree: tree!)

	}

	init(readFrom: BitInputStream) {
		tree = readTree(reader: readFrom)
	}

	func compress(readFrom: BitInputStream, writeTo: BitOutputStream) {
		tree!.writeTo(writer: writeTo)
		writeSymbols(reader: readFrom, writer: writeTo)
		writeTo.close()
	}

	func decompress(readFrom: BitInputStream, writeTo: BitOutputStream) {
		let len = readFrom.readInt()
		for _ in 0..<len {
			writeTo.writeByte(byte: UInt16(tree!.readChar(reader: readFrom)))
		}
		writeTo.close()
	}

	func readTree(reader: BitInputStream) -> Tree {
		let flag = reader.readBool()
		if flag {
			return Leaf(frq: 0, sym: reader.readChar())
		} else {
			let l = readTree(reader: reader)
			let r = readTree(reader: reader)
			return Node(left: l, right: r)
		}
	}

	func calcFrecuencies(source: BitInputStream) -> [Int]{
		var freqs = [Int](repeating: 0, count: MAX_SYMBOLS)
		let bytes = source.getBytes()
		for i in 0 ..< bytes.count {
			let c = bytes[i]
			freqs[Int(c)] += 1
		}
		return freqs
	}

	func buildCodes(tree: Tree) -> [String] {
		var codes = [String](repeating: "", count: MAX_SYMBOLS)
		tree.dumpCodes(codes: &codes, prefix: "")
		return codes
	}

	func writeSymbols(reader: BitInputStream, writer: BitOutputStream) {
		let bytes = reader.getBytes()
		let lbytes = bytes.count
		writer.writeInt(i: UInt32(lbytes))
		for (_, s) in bytes.enumerated() {
			let code = codes[Int(s)]
			for c in code {
				writer.writeBitc(bit: c)
			}
		}
	}
}
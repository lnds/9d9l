import Foundation

func _panic(msg : String) {
	print(msg)
	exit(-1)
}

class BitInputStream {
	var buffer : UInt16 = 0
	var pos : Int = 0
	var bitsInBuffer : UInt16 = 0
	var eof : Bool = false
	var bytes : [UInt8] = [UInt8]()
	var len : Int = 0

	init(inputFile: String) {
		if let data = NSData(contentsOfFile: inputFile) {
			bytes = [UInt8](repeating: 0, count: data.length)
			data.getBytes(&bytes, length: data.length)
			len = data.length
		}
		fillBuffer()
	}

	func readBool() -> Bool {
		if eof {
			_panic(msg: "can't read bool from input stream")
		}

		bitsInBuffer -= 1

		let bit = ((buffer >> bitsInBuffer) & 1) == 1
		if bitsInBuffer == 0 {
			fillBuffer()
		}
		return bit
	}

	func readChar() -> UInt8 {
		if eof {
			_panic(msg: "reading from empty input stream")
		}

		var x = buffer
		if bitsInBuffer == 8 {
			fillBuffer()
		} else {
			x = x << (8 - bitsInBuffer)
			let temp = bitsInBuffer
			fillBuffer()
			bitsInBuffer = temp
			x |= buffer >> bitsInBuffer
		}
		return UInt8(x & 0xFF)
	}

	func readInt() -> UInt32 {
		var x = UInt32(readChar())
		x = (x << 8) | UInt32(readChar())
		x = (x << 8) | UInt32(readChar())
		x = (x << 8) | UInt32(readChar())
		return x
	}

	func fillBuffer() {
		if pos == len {
			eof = true
		} else {
			buffer = UInt16(bytes[pos])
			bitsInBuffer = 8
			pos += 1
		}
	}

	func getBytes() -> [UInt8] {
		return bytes
	}
}

class BitOutputStream {
	var out : Data

	var buffer : UInt16 = 0
	var bitsInBuffer : UInt16 = 0
	var output : String

	init(outputFile: String) {
		out = Data()
		output = outputFile
	}

	func writeBitc(bit: Character) {
		if bit == "0" {
			writeBit(bit: 0)
		} else if bit == "1" {
			writeBit(bit: 1)
		} else {
			_panic(msg: "argument must be '0' or '1'")
		}
	}

	func writeBit(bit: UInt16) {
		if bit != 0 && bit != 1 {
			_panic(msg: "argument must be 0 or 1")
		}
		buffer = (buffer << 1) | bit
		bitsInBuffer += 1
		if bitsInBuffer == 8 {
			clearBuffer()
		}
	}

	func writeByte(byte: UInt16) {
		for i in 0..<8 {
			let bit = (byte >> (8 - i - 1)) & 1
			writeBit(bit: bit)
		}
	}

	func writeInt(i: UInt32) {
		writeByte(byte: UInt16((i >> 24) & 0xFF))
		writeByte(byte: UInt16((i >> 16) & 0xFF))
		writeByte(byte: UInt16((i >>  8) & 0xFF))
		writeByte(byte: UInt16((i >>  0) & 0xFF))

	}

	func clearBuffer() {
		if bitsInBuffer == 0 { return }
		if bitsInBuffer > 0 {
			buffer = buffer << (8 - bitsInBuffer)
		}
		out.append(UInt8(buffer))
		buffer = 0
		bitsInBuffer = 0
	}

	func close() {
		let nsdata = NSData(data: out)
		nsdata.write(toFile: output, atomically: true)
	}
}
import Foundation

func usage() {
	print("Uso: huffman [-c|-d] archivo_entrada archivo_salida")
	exit(-1)
}

func comprimir(input: String, output: String) {
	let reader = BitInputStream(inputFile: input)
	let writer = BitOutputStream(outputFile: output)
	let tree = HuffTree(buildFrom: reader)
	tree.compress(readFrom: reader, writeTo: writer)

}

func descomprimir(input: String, output: String) {
	let reader = BitInputStream(inputFile: input)
	let writer = BitOutputStream(outputFile: output)
	let tree = HuffTree(readFrom: reader)
	tree.decompress(readFrom: reader, writeTo: writer)
}

let args = ProcessInfo.processInfo.arguments
let argc = ProcessInfo.processInfo.arguments.count

if argc != 4 {
	usage()
} else {
	if args[1] == "-c" {
		comprimir(input: args[2], output: args[3])
	}
	else if args[1] == "-d" {
		descomprimir(input: args[2], output: args[3])
	}
	else {
		usage()
	}
}

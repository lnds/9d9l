package main

import (
	"fmt"
	"os"
	. "github.com/lnds/9d9l/huffman/tree"
	. "github.com/lnds/9d9l/huffman/io"
)

func usage() {
	fmt.Println("Uso: huffman [-c|-d] archivo_entrada archivo_salida")
	os.Exit(0)
}

func compress(input string, output string) {
	reader := NewBitInputStream(input)
	writer := NewBitOutputStream(output)
	tree := BuildHuffTree(reader)
	tree.Compress(reader, writer)
}

func decompress(input string, output string) {
	reader := NewBitInputStream(input)
	writer := NewBitOutputStream(output)
	tree := ReadHuffTree(reader)
	tree.Decompress(reader, writer)
}

func main() {

	if len(os.Args) != 4 {
		usage()
	}

	if os.Args[1] == "-c" {
		compress(os.Args[2], os.Args[3])
	} else if os.Args[1] == "-d" {
		decompress(os.Args[2], os.Args[3])
	} else {
		usage()
	}
}

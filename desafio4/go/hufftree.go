package main

import("fmt")

const MAX_SYMBOLS = 256
type Tree interface {
	Freq() uint
	WriteTo(writer *BitOutputStream)
	DumpCodes(codes []string, prefix string)
	ReadChar(reader *BitInputStream) byte
}

type Leaf struct {
	freq uint
	symbol byte
}

type Node struct {
	freq uint
	left Tree
	right Tree
}

type HuffTree struct {
	tree Tree
	codes []string
}

func NewLeaf(f uint, sym byte) *Leaf {
	p := new(Leaf)
	p.freq = f
	p.symbol = sym
	return p
}

func NewNode(f uint, l Tree, r Tree) *Node {
	p := new(Node)
	p.freq = l.Freq() + r.Freq()
	p.left = l
	p.right = r
	return p
}

func (l Leaf) Freq() uint {
	return l.freq
}

func (n Node) Freq() uint {
	return n.freq
}

func (l Leaf) WriteTo(writer *BitOutputStream) {
	writer.WriteBit(1)
	writer.WriteByte(uint16(l.symbol))
}

func (n Node) WriteTo(writer *BitOutputStream) {
	writer.WriteBit(0)
	n.left.WriteTo(writer)
	n.right.WriteTo(writer)
}

func (l Leaf) DumpCodes(codes []string, prefix string) {
	codes[l.symbol] = prefix
}

func (n Node) DumpCodes(codes []string, prefix string) {
	n.left.DumpCodes(codes, prefix+"0")
	n.right.DumpCodes(codes, prefix+"1")
}

func (l Leaf) ReadChar(reader *BitInputStream) byte {
	return l.symbol
}

func (n Node) ReadChar(reader *BitInputStream) byte {
	if reader.ReadBool() {
		return n.right.ReadChar(reader)
	} else {
		return n.left.ReadChar(reader)
	}
}

func BuildHuffTree(reader *BitInputStream) *HuffTree {
	p := new(HuffTree)

	freqs := calcFrecuencies(reader)
	heap := NewHeap(MAX_SYMBOLS)
	for s, f := range(freqs) {
		if f > 0 {
			heap.Insert(NewLeaf(uint(f), byte(s)))
		}
	}
	
	for heap.Size() > 1 {
		l := heap.Extract()
		r := heap.Extract()
		heap.Insert(NewNode(l.Freq()+r.Freq(), l, r))
	}
	tree := heap.Extract()
	codes := buildCodes(tree)
	p.tree = tree
	p.codes = codes
	return p
}

func buildCodes(tree Tree) []string {
	prefix := ""
	codes := make([]string, MAX_SYMBOLS)
	tree.DumpCodes(codes, prefix)
	return codes
}

func (h *HuffTree) Compress(reader *BitInputStream, writer *BitOutputStream) {
	h.tree.WriteTo(writer)
	h.writeSymbols(reader, writer)
	writer.Close()
}

func (h *HuffTree) Decompress(reader *BitInputStream, writer *BitOutputStream) {
	len := reader.ReadInt()
	fmt.Printf("decompress len = %d\n", len)
	for i := uint32(0); i < len; i++ {
		writer.WriteByte(uint16(h.tree.ReadChar(reader)))
	}
	writer.Close()
}

func (h HuffTree) writeSymbols(reader *BitInputStream, writer *BitOutputStream) {
	bytes := reader.GetBytes()
	lbytes := uint32(len(bytes))
	writer.WriteInt(lbytes)
	for _, s := range(bytes) {
		code := h.codes[s]
		for _, c := range(code) {
			writer.WriteBit(uint16(int(c) - '0'))
		}
	}
}


func ReadHuffTree(reader *BitInputStream) *HuffTree {
	p := new(HuffTree)
	p.tree = readTree(reader)
	return p	
}

func readTree(reader *BitInputStream) Tree {
	flag := reader.ReadBool()
	if flag {
		return *NewLeaf(0, reader.ReadChar())
	} else {
		l := readTree(reader)
		r := readTree(reader)
		return *NewNode(0, l, r)
	}
}

func calcFrecuencies(reader *BitInputStream) []uint {
	freqs := make([]uint, MAX_SYMBOLS)
	bytes := reader.GetBytes()
	for _, c := range(bytes) {
		freqs[c] += 1
	}
	return freqs
}
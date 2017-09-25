package main

import ("os"; "log"; "io/ioutil"; "bufio"; "fmt")

type BitInputStream struct {
	buffer uint16
	pos  int
	bitsInBuffer  uint16
	eof  bool
	bytes  []byte
	len  int
}

type BitOutputStream struct {
	out *bufio.Writer
	buffer uint16
	bitsInBuffer uint16
}

func NewBitInputStream(filename string) *BitInputStream {
	p := new(BitInputStream)
	p.buffer = 0
	p.pos = 0
	p.bitsInBuffer = 0
	p.eof = false

	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	bytes, err := ioutil.ReadAll(file)
	if err != nil {
		log.Fatal(err)
	}
	p.bytes = bytes
	p.len = len(bytes)
	p.fillBuffer()
	return p
}

func NewBitOutputStream(filename string) *BitOutputStream {
	p := new(BitOutputStream)
	file, err := os.Create(filename)
	if err != nil {
		log.Fatal(err)
	}
	p.out = bufio.NewWriter(file)
	p.buffer = 0
	p.bitsInBuffer = 0
	return p
}

func (i *BitInputStream) ReadBool() bool {
	if i.eof { log.Fatal("can't read bool from input stream") }

	i.bitsInBuffer -= 1

	bit := ((i.buffer >> i.bitsInBuffer) & 1) == 1
	if i.bitsInBuffer == 0 {
		i.fillBuffer()
	}
	return bit
}

func (i *BitInputStream) ReadChar() byte {
	if i.eof { log.Fatal("reading from empty input stream") }

	fmt.Printf("read char pos = %d\n", i.pos)

	x := i.buffer
	if i.bitsInBuffer == 8 {
		i.fillBuffer()
	} else {
		x = x << (8 - i.bitsInBuffer)
		temp := i.bitsInBuffer
		i.fillBuffer()
		i.bitsInBuffer = temp
		x |= i.buffer >> i.bitsInBuffer
	}
	return byte(x & 0xFF)
}

func (i *BitInputStream) ReadInt() uint32 {
	fmt.Printf("read int pos = %d\n", i.pos)
	x := uint32(i.ReadChar())
	x = (x << 8) | uint32(i.ReadChar())
	x = (x << 8) | uint32(i.ReadChar())
	x = (x << 8) | uint32(i.ReadChar())
	return x
}

func (i *BitInputStream) GetBytes() []byte {
	return i.bytes
}

func (i *BitInputStream) fillBuffer() {
	if i.pos == i.len {
		i.eof = true
	} else {
		i.buffer = uint16(i.bytes[i.pos])
		i.bitsInBuffer = 8
		i.pos++
	}
}

func (o *BitOutputStream) clearBuffer() {
	if o.bitsInBuffer == 0 { return }
	if o.bitsInBuffer > 0 {
		o.buffer = o.buffer << (8 - o.bitsInBuffer)
	}
	o.out.WriteByte(byte(o.buffer))
	o.buffer = 0
	o.bitsInBuffer = 0
}

func (o *BitOutputStream) WriteBit(bit uint16) {
	if bit != 0 && bit != 1 { log.Fatal("argument must be 0 or 1.") }
	o.buffer = (o.buffer << 1) | bit
	o.bitsInBuffer++
	if o.bitsInBuffer == 8 {
		o.clearBuffer()
	}
}

func (o *BitOutputStream) WriteByte(b uint16) {
	for i := 0; i < 8; i++ {
		bit := (b >> (8 - uint16(i) - 1)) & 1
		o.WriteBit(bit)
	}
}

func (o *BitOutputStream) WriteInt(i uint32) {
	o.WriteByte(uint16((i >> 24) & 0xFF))
	o.WriteByte(uint16((i >> 16) & 0xFF))
	o.WriteByte(uint16((i >>  8) & 0xFF))
	o.WriteByte(uint16((i >>  0) & 0xFF))
}

func (o *BitOutputStream) Close() {
	o.clearBuffer()
	o.out.Flush()
}
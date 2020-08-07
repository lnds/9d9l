use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;

pub struct BitInputStream {
    buffer: u16,
    pos: usize,
    bits_in_buffer: i16,
    eof: bool,
    bytes: Vec<u8>,
    len: usize,
}

pub struct BitOutputStream {
    out: BufWriter<File>,
    buffer: u16,
    bits_in_buffer: i16,
}

impl BitInputStream {
    pub fn new(file: &str) -> BitInputStream {
        let mut bis = BitInputStream {
            buffer: 0,
            pos: 0,
            bits_in_buffer: 0,
            eof: false,
            bytes: match File::open(file) {
                Err(e) => panic!("No pudo abrir archivo {}, causa: {}", file, e.description()),
                Ok(mut f) => {
                    let mut buffer = Vec::new();
                    f.read_to_end(&mut buffer).unwrap();
                    buffer
                }
            },
            len: 0,
        };
        bis.len = bis.bytes.len();
        bis.fill_buffer();
        bis
    }

    pub fn get_bytes(&mut self) -> &Vec<u8> {
        &self.bytes
    }

    pub fn read_char(&mut self) -> u8 {
        if self.eof {
            panic!("reading from empty input stream");
        }

        let mut x = self.buffer;
        if self.bits_in_buffer == 8 {
            self.fill_buffer();
        } else {
            x <<= 8 - self.bits_in_buffer;
            let temp = self.bits_in_buffer;
            self.fill_buffer();
            self.bits_in_buffer = temp;
            x |= self.buffer >> self.bits_in_buffer;
        }
        (x & 0xFF) as u8
    }

    pub fn read_bool(&mut self) -> bool {
        if self.eof {
            panic!("reading from empty input stream");
        }

        self.bits_in_buffer -= 1;
        let bit = ((self.buffer >> self.bits_in_buffer) & 1) == 1;
        if self.bits_in_buffer == 0 {
            self.fill_buffer();
        }
        bit
    }

    pub fn read_int(&mut self) -> u32 {
        let mut x: u32 = u32::from(self.read_char());
        x = (x << 8) | u32::from(self.read_char());
        x = (x << 8) | u32::from(self.read_char());
        x = (x << 8) | u32::from(self.read_char());
        x
    }

    fn fill_buffer(&mut self) {
        if self.pos == self.len {
            self.eof = true;
        } else {
            self.buffer = u16::from(self.bytes[self.pos]);
            self.bits_in_buffer = 8;
            self.pos += 1;
        }
    }
}

impl BitOutputStream {
    pub fn new(output: &str) -> BitOutputStream {
        BitOutputStream {
            out: BufWriter::new(File::create(output).unwrap()),
            buffer: 0,
            bits_in_buffer: 0,
        }
    }

    pub fn write_bit(&mut self, bit: u8) {
        if bit != 0 && bit != 1 {
            panic!("argument must be 0 or 1., received {}", bit);
        }
        self.buffer = (self.buffer << 1) | (u16::from(bit));
        self.bits_in_buffer += 1;
        if self.bits_in_buffer == 8 {
            self.clear_buffer();
        }
    }

    pub fn write_int(&mut self, i: u32) {
        self.write_byte(((i >> 24) & 0xFF) as u16);
        self.write_byte(((i >> 16) & 0xFF) as u16);
        self.write_byte(((i >> 8) & 0xFF) as u16);
        self.write_byte((i & 0xFF) as u16);
    }

    pub fn write_byte(&mut self, byte: u16) {
        for i in 0..8 {
            let bit = (byte >> (8 - i - 1)) & 1;
            self.write_bit(bit as u8);
        }
    }

    fn clear_buffer(&mut self) {
        if self.bits_in_buffer == 0 {
            return;
        }
        if self.bits_in_buffer > 0 {
            self.buffer <<= 8 - self.bits_in_buffer;
        }
        self.out.write_all(&[self.buffer as u8]).unwrap();
        self.buffer = 0;
        self.bits_in_buffer = 0;
    }

    pub fn close(&mut self) {
        self.clear_buffer();
        self.out.flush().unwrap();
    }
}

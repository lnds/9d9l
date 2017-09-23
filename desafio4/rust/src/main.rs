mod huffman;

use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;
use std::error::Error;
use huffman::MAX_SYMBOLS;
use huffman::HuffTree;
use huffman::io::{BitInputStream, BitOutputStream};


fn usage() {
	println!("Uso: huffman [-c|-d] archivo_entrada archivo_salida")
}

fn process(args:Vec<String>) {
	match args[1].trim() {
		"-c" => compress(&args[2], &args[3]),
		"-d" => decompress(&args[2], &args[3]),
		_ => usage()
	} 
}

fn compress(input:&String, output:&String) {
	let bytes = match File::open(input) {
		Err(e) => panic!("No pudo crear archivo {}, causa: {}", input, e.description()),
		Ok(mut f) => {
			let mut buffer = Vec::new();
			f.read_to_end(&mut buffer).unwrap();
			buffer
		}
	};
	let freqs = calc_frecuencies(&bytes);
	let mut tree = HuffTree::build(freqs);
	let codes = HuffTree::build_codes(&tree);
	let mut writer = BitOutputStream::new(output);
	tree.write_to(&mut writer);
	let len = bytes.len();
	writer.write_int(len as u32);
	for &symbol in bytes.iter() {
		let code = &codes[symbol as usize];
		for c in code.chars() {
			writer.write_bit(c.to_digit(2).unwrap() as u8);
		}
	}
	writer.flush();
}


fn decompress(input:&String, output:&String) {
	let mut reader = BitInputStream::new(&input);
	let mut writer = BufWriter::new(File::create(output).unwrap());
	let mut tree = HuffTree::read(&mut reader);
	let len = tree.read_length(&mut reader);
	for _ in 0..len {
		writer.write(&tree.read_char(&mut reader)).unwrap();
	} 
}

fn calc_frecuencies(bytes:&[u8]) -> [usize; MAX_SYMBOLS] {
	let mut freqs : [usize; MAX_SYMBOLS] = [0; MAX_SYMBOLS];
	for &b in bytes.iter() {
		freqs[b as usize] += 1;
	}
	freqs
}


fn main() {
    let args: Vec<String> = env::args().collect();
	if args.len() != 4 {
		usage()
	} else {
		process(args)
	}
}

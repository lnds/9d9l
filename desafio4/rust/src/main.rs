mod huffman;

extern crate lzw;
use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;
use std::error::Error;
use lzw::MsbWriter;
use lzw::BitWriter;
use huffman::MAX_SYMBOLS;
use huffman::HuffTree;



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
	let mut writer = MsbWriter::new(BufWriter::new(File::create(output).unwrap()));
	tree.write_to(&mut writer);
	for &symbol in bytes.iter() {
		let code = &codes[symbol as usize];
		for c in code.chars() {
			writer.write_bits(c.to_digit(2).unwrap() as u16, 1).unwrap();
		}
	}
}


fn decompress(input:&String, output:&String) {
	println!("descomprimir {} en {}", input, output);
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

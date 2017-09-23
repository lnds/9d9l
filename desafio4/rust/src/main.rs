mod huffman;

use std::env;
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
	let mut reader = BitInputStream::new(&input);
	let mut tree = HuffTree::build(&mut reader);
	let mut writer = BitOutputStream::new(output);
	tree.compress(&mut reader, &mut writer);
}


fn decompress(input:&String, output:&String) {
	let mut reader = BitInputStream::new(&input);
	let mut writer = BitOutputStream::new(&output);
	let mut tree = HuffTree::read(&mut reader);
	tree.decompress(&mut reader, &mut writer);
}

fn main() {
    let args: Vec<String> = env::args().collect();
	match args.len() {
		4 => process(args), 
		_ => usage()
	}
}

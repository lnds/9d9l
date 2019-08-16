pub mod io;

use io::{BitInputStream, BitOutputStream};

pub const MAX_SYMBOLS: usize = 256;

#[derive(Clone)]
enum Tree {
	Leaf(usize, u8),
	Node(usize, Box<Tree>, Box<Tree>)
}

fn freq_tree(t:&Tree) -> usize {
	match *t {
		Tree::Leaf(f, _) => f,
		Tree::Node(f, _, _)=> f
	}
}

fn freq(t:&Option<Tree>) -> usize {
	match *t {
		None => 0,
		Some(ref t) => freq_tree(t)
	}
}

fn dump_codes(tree:&Tree, codes: &mut Vec<String>, prefix: &mut String) {
	match *tree {
		Tree::Leaf(_, sym) =>
			codes[sym as usize] = prefix.clone(),
		Tree::Node(_, ref left, ref right) => {
			prefix.push('0');
			dump_codes(left, codes, prefix);
			prefix.pop();
			prefix.push('1');
			dump_codes(right, codes, prefix);
			prefix.pop();
		}
	}
}

fn write_tree(tree:&Tree, writer:&mut BitOutputStream) {
	match *tree {
		Tree::Leaf(_, sym) => {
			writer.write_bit(1);
			writer.write_byte(u16::from(sym));
		}
		Tree::Node(_, ref left, ref right) => {
			writer.write_bit(0);
			write_tree(left, writer);
			write_tree(right, writer);
		}
	}
}

fn read_char(tree: &Tree, reader: &mut BitInputStream) -> u8 {
	match *tree {
		Tree::Leaf(_, sym) => {
			sym
		}
		Tree::Node(_, ref left, ref right) => {
			if reader.read_bool() {
				read_char(right, reader)
			} else {
				read_char(left, reader)
			}
		}
	}
}

struct Heap {
	data: Vec<Option<Tree>>,
	last: usize
}

impl Heap {

	pub fn new(size:usize) -> Heap {
		Heap {
			data : vec![None;size+1],
			last : 0
		}
	}

	pub fn insert(&mut self, elem:Tree) {
		self.last += 1;
		self.data[self.last] = Some(elem);
		let mut j = self.last;
		while j > 1 {
			if freq(&self.data[j]) < freq(&self.data[j/2]) {
				self.data.swap(j, j/2);
			} 
			j /= 2;
		}
	}

	pub fn extract(&mut self) -> Option<Tree> {
		if self.last == 0 {
			None
		} else {
			let min = self.data[1].clone();
			self.data[1] = self.data[self.last].clone();
			self.last -= 1;
			let mut j = 1;
			while 2 * j <= self.last {
				let mut k = 2 * j;
				if k < self.last && freq(&self.data[k+1]) < freq(&self.data[k]) {
					k += 1;
				}
				if freq(&self.data[j]) < freq(&self.data[k]) {
					break;
				}
				self.data.swap(j, k);
				j = k;
			}
			min
		}
	}

	pub fn size(&mut self) -> usize {
		self.last
	}
}

pub struct HuffTree {
	tree: Tree,
	codes: Vec<String>
}

impl HuffTree {

	pub fn build(mut reader: &mut BitInputStream) -> HuffTree {
		let freqs :[usize; MAX_SYMBOLS] = HuffTree::calc_frecuencies(&mut reader);
		let mut heap = Heap::new(MAX_SYMBOLS);
		for (s, &f) in freqs.iter().enumerate() {
			if f > 0 {
				heap.insert(Tree::Leaf(f, s as u8));
			}
		}
		while heap.size() > 1 {
			let l = heap.extract().unwrap();
			let r = heap.extract().unwrap();
			heap.insert(Tree::Node(freq_tree(&l)+freq_tree(&r), Box::new(l), Box::new(r)))
		}
		let tree = heap.extract().unwrap();
		let codes = HuffTree::build_codes(&tree);
		HuffTree {
			tree,
			codes
		}
	}

	fn read_tree(reader: &mut BitInputStream) -> Tree {
		let flag = reader.read_bool();
		if flag {
			Tree::Leaf(0, reader.read_char())
		} else {
			let l = HuffTree::read_tree(reader);
			let r = HuffTree::read_tree(reader);
			Tree::Node(0, Box::new(l), Box::new(r))
		}
	}

	pub fn read(reader: &mut BitInputStream) -> HuffTree {
		HuffTree {
			tree: HuffTree::read_tree(reader),
			codes: vec![]
		}
	}
	pub fn write_to(&mut self, writer: &mut BitOutputStream) {
		write_tree(&self.tree, writer);
	}

	pub fn write_symbols(&mut self, reader: &mut BitInputStream, writer: &mut BitOutputStream) {
		let bytes = reader.get_bytes();
		let len = bytes.len();
		writer.write_int(len as u32);
		for &symbol in bytes.iter() {
			let code = &self.codes[symbol as usize];
			for c in code.chars() {
				writer.write_bit(c.to_digit(2).unwrap() as u8);
			}
		}
	}

	pub fn compress(&mut self, mut reader: &mut BitInputStream, mut writer: &mut BitOutputStream) {
		self.write_to(&mut writer);
		self.write_symbols(&mut reader, &mut writer);
		writer.close();
	}

	pub fn decompress(&mut self, reader: &mut BitInputStream, writer: &mut BitOutputStream) {
		let len = reader.read_int() as usize;
		for _ in 0..len {
			writer.write_byte(u16::from(read_char(&self.tree, reader)));
		} 
	}

	fn build_codes(tree: &Tree) -> Vec<String> {
		let mut prefix:String = "".into();
		let mut codes: Vec<String> = vec!["".into(); MAX_SYMBOLS];
		dump_codes(&tree, &mut codes, &mut prefix);
		codes.clone()
	}

	fn calc_frecuencies(reader: &mut BitInputStream) -> [usize; MAX_SYMBOLS] {
		let mut freqs : [usize; MAX_SYMBOLS] = [0; MAX_SYMBOLS];
		let bytes = reader.get_bytes();
		for &b in bytes.iter() {
			freqs[b as usize] += 1;
		}
		freqs
	}
}


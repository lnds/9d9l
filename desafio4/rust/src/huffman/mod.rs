
use lzw::BitWriter;


pub const MAX_SYMBOLS: usize = 256;

#[derive(Clone)]
enum Tree {
	Leaf(usize, u8),
	Node(usize, Box<Tree>, Box<Tree>)
}

fn freq_tree(t:&Tree) -> usize {
	match *t {
		Tree::Leaf(f,_) => f,
		Tree::Node(f,_,_)=> f
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

fn write_tree(tree:&Tree, writer:&mut BitWriter) {
	match *tree {
		Tree::Leaf(_, sym) => {
			writer.write_bits(1, 1).unwrap();
			writer.write_bits(sym as u16, 8).unwrap();
		}
		Tree::Node(_, ref left, ref right) => {
			writer.write_bits(0, 1).unwrap();
			write_tree(left, writer);
			write_tree(right, writer);
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
				if k + 1 <= self.last && freq(&self.data[k+1]) < freq(&self.data[k]) {
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

}


impl HuffTree {


	pub fn build(freqs:[usize; MAX_SYMBOLS]) -> HuffTree {
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
		HuffTree {
			tree: heap.extract().unwrap()
		}
	}

	pub fn build_codes(tree:&HuffTree) -> Vec<String> {
		let mut prefix:String = "".into();
		let mut codes: Vec<String> = vec!["".into(); MAX_SYMBOLS];
		dump_codes(&tree.tree, &mut codes, &mut prefix);
		codes.clone()
	}

	pub fn write_to(&mut self, writer: &mut BitWriter) {
		write_tree(&self.tree, writer);
	}

}


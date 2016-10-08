use std::env;
use std::error::Error;
use std::io::BufReader;
use std::io::BufRead;
use std::io::LineWriter;
use std::io::Write;
use std::fs::File;
use std::str;
use std::ptr;

const POS_VECTOR: usize = 9;
const ELEMENTOS_VECTOR: usize = 23;
const TAM_PERIODO: usize = 6;
const CANT_INSTITUCIONES: usize = 6;
const TAM_VECTOR: usize = ELEMENTOS_VECTOR * TAM_PERIODO;
const TAM_VECTOR_ENTRADA: usize = TAM_VECTOR * CANT_INSTITUCIONES;
const LARGO_LINEA: usize = POS_VECTOR + TAM_VECTOR_ENTRADA;
const TAM_SALIDA: usize = POS_VECTOR + 1 + TAM_VECTOR;


fn ordenar_vector(vector:&[u8],  result:&mut [u8]) {
	let mut n = 0;
	let cero = ['0' as u8; TAM_PERIODO];
	for p in vector.chunks(TAM_PERIODO) {

		if p == cero { continue; }

		let mut i = 0;
		let mut q = 1;
		while i < n && p < &result[q..q+TAM_PERIODO] { i += 1; q += TAM_PERIODO; } // busca si p está en el arreglo

		if i < n && p == &result[q..q+TAM_PERIODO] { continue; } // si ya existe lo ignora

		// inserta p en el arreglo
		if i == n {
			if n < ELEMENTOS_VECTOR {
				let q = n * TAM_PERIODO+1;
				result[q..q+TAM_PERIODO].clone_from_slice(p);
			}
		} else {
			for j in (i+1..ELEMENTOS_VECTOR).rev() {
				let q = j*TAM_PERIODO+1;
				unsafe {
					ptr::copy_nonoverlapping(&mut result[q-TAM_PERIODO], &mut result[q], TAM_PERIODO)
				}
			}
			let q = i*TAM_PERIODO+1;
			result[q..q+TAM_PERIODO].clone_from_slice(p);
		}
		n += 1;
		// si excedimos el tamaño del vector salimos
		if n > ELEMENTOS_VECTOR { break; }
	}

	// retorna el resultado
	if n == 0 {
		result[0] = 'N' as u8;
	} else if n > ELEMENTOS_VECTOR {
		result[0] = 'S' as u8;
		for j in 1..result.len() {
			result[j] = ' ' as u8
		}
	} else {
		result[0] = 'D' as u8;
	}
}


fn procesar_linea(buf: &Vec<u8>) -> [u8; TAM_SALIDA] {
	let mut res : [u8; TAM_SALIDA] = [' ' as u8; TAM_SALIDA];
	res[0..POS_VECTOR].clone_from_slice(&buf[0..POS_VECTOR]);
	ordenar_vector(&buf[POS_VECTOR..], &mut res[POS_VECTOR..]); 
	return res;
}


fn main() {
	let args: Vec<String> = env::args().collect();
	if args.len() != 3 {
		println!("Uso: ordena_vector archivo_entrada archivo_salida");
		return
	} 

    let t0 = std::time::Instant::now();

	let entrada = match File::open(&args[1]) {
		Err(e) => panic!("No pudo abrir archivo {}, causa: {}", args[1], e.description()),
		Ok(file) => BufReader::new(file)
	};
	let mut salida = match File::create(&args[2]) {
		Err(e) => panic!("No pudo crear archivo {}, causa: {}", args[2], e.description()),
		Ok(file) => LineWriter::new(file)
	};

	for (num_linea, linea) in entrada.lines().enumerate() {
		let buf = &linea.unwrap().into_bytes();
		
		if buf.len() != LARGO_LINEA {
			println!("Error en el largo de la linea {}", num_linea);
			writeln!(salida, "{}", str::from_utf8(&buf).unwrap()).unwrap();
		} else {
			let vector = procesar_linea(buf);
			writeln!(salida, "{}", str::from_utf8(&vector).unwrap()).unwrap();
		};

	}

	let dur  = t0.elapsed();
	let secs = dur.as_secs();
    let frac = dur.subsec_nanos()/1000000;
    println!("tiempo ocupado: {}.{}s", secs, frac);
}

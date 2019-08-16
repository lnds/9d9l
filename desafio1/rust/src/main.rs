extern crate rand;
use std::io;
use rand::Rng;

enum Accion { 
	Finalizar, 
	Error, 
	Jugar(Vec<u32>) 
}

use Accion::*;

fn main() {
    let max = 5; // largo de la secuencia
    mostrar_reglas(max);
    let secuencia = generar_secuencia(max);
	let mut intentos = 0;
	loop {
		intentos += 1;
		let accion = ingresar(max);
	    match accion {
	    	Error => { println!("error!\n") }
	    	Finalizar => { 
	    		println!("\ngracias por jugar, adios.");
	    		break 
			}
	    	Jugar(numero) => {
			    println!("tu ingresaste {:?}",numero);
	    		let (toques, famas)  = comparar(&secuencia, &numero);
	    		println!("resultado: {} Toques {} Famas\n", toques, famas);
	    		if famas == max {
	    			println!("Ganaste! Acertaste al intento {}! La secuencia era {:?}.", intentos, secuencia);
	    			break 
				}
	    	}
		}	
	}
}

fn mostrar_reglas(tam: usize) {
	println!("\
		Bienvenido a Toque y Fama.\n\
		==========================\n\n\
		En este juego debes tratar de adivinar una secuencia de {} dígitos generadas por el programa.\n\
		Para esto ingresas {} dígitos distintos con el fin de adivinar la secuencia.\n\
		Si has adivinado correctamente la posición de un dígito se produce una Fama.\n\
		Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n\n\
		Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:\n\
		tu ingresaste [4, 0, 8, 6, 3]\n\
		resultado: 2 Toques 2 Famas\n\n", tam, tam);
}

fn ingresar(tam: usize) -> Accion {
	println!("Ingresa una secuencia de {} dígitos distintos (o escribe salir):", tam);
	
	let mut accion = String::new();
	io::stdin().read_line(&mut accion).expect("No pudo leer linea."); 
	let accion = accion.trim().to_string();
	if accion == "salir" || accion == "" { return Accion::Finalizar }
	validar_entrada(tam, &accion)
}

fn validar_entrada(tam: usize, accion: &str) -> Accion {
	let mut num = Vec::new();
	for (i,c) in accion.chars().enumerate() {
		if !c.is_digit(10) || i >= tam {  return Error } 
		else {
			let digito = c.to_digit(10).unwrap();
			if num.contains(&digito) { 
				return Error 
			}
			num.push(digito)
		}
	} 
	if num.len() == tam { 
		Jugar(num) 
	} 
	else { 
		Error 
	}
}

fn comparar(sec: &[u32], num: &[u32]) -> (usize, usize) {
	let mut toques = 0;
	let mut famas = 0;
	for (i, n) in num.iter().enumerate() {
		for (j, m) in sec.iter().enumerate() {
			if n == m {
				if i == j { famas += 1 }
				else { toques += 1 }
			}
		}
	}
	(toques, famas)
}

fn generar_secuencia(tam:usize) -> Vec<u32> {
	let mut rng = rand::thread_rng();
	let mut digitos : Vec<u32> = (0..10).collect();
	rng.shuffle(&mut digitos);
	digitos.truncate(tam);
	digitos
}

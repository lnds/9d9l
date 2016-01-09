# Toque y fama en Go

Rust es un lenguaje creado por Mozilla cuyo objetivo es producir código seguro.


En Rust no hay Garbage Collection, todo el manejo de memoria es estático. 
Rust tiene estrictas reglas para el manejo de memoria y el acceso a estas.
El objetivo es evitar punteros perdidos, acceder a memoria ya liberada, etc.

Este desafío no es un buen ejemplo para estas características de Rust, pero si pude aprovechar los Enum, que son una forma de tipos algebraicos.

Fíjate en esta declaración

	enum Accion { Finalizar, Error, Jugar(Vec<u32>) }

Accion es un tipo algebraico que puede tomar los valores Finalizar, Error o Jugar(Vec<u32>). 

Esto se usa en el nucleo principal del programa:

	let accion = ingresar(max);
    match accion {
    	Error => { println!("error!\n") }
    	Finalizar => { 
    		println!("\ngracias por jugar, adios.");
    		break }
    	Jugar(numero) => {
		    println!("tu ingresaste {:?}",numero);
    		let (toques, famas)  = comparar(&secuencia, &numero);
    		println!("resultado: {} Toques {} Famas\n", toques, famas);
    		if famas == max {
    			println!("Ganaste! Acertaste al intento {}! La secuencia era {:?}.", intentos, secuencia);
    			break }
    	}
	}	

la función ingresar() retorna una de 3 posibles acciones que corresponden a los valores del enum Accion.
Lo interesante es que si la accion es Jugar esta contiene el numero ingresado por el usuario, expresado como un vector con cada uno de los dígitos.

## ¿Donde aprender Rust?

	- Por ahora el sitio de Rust tiene información.
	- Hay una buena presentación de Jehuda Katz en en InfoQ: http://www.infoq.com/presentations/rust-gc

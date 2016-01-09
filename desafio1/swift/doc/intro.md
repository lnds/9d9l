# Toque y Fama

Swift es el lenguaje desarrollado por Apple, como posible sucesor de Objective-C.

Swift es opensource y se puede obtener desde swift.org y corre en Linux y Mac OSX.

Es un lenguaje que tiene muchos elementos de otros. Es un heredero de la familia de lenguajes C y tal como Go y Rust, se trata de simplificar la sintáxis con respecto a C (o Java).

En un primer vistazo la sintáxis de Go y Swift se parecen

Veamos como se implementa la función que compara los números ingresados por el jugador y la secuencia generada aleatoreamente:


	func comparar(num:[Int], sec:[Int]) -> (toques:Int, famas:Int) {
		var toques = 0
		var famas = 0
		for (i, n) in num.enumerate() {
			for (j, m) in sec.enumerate() {
				if n == m {
					if i == j {
						famas += 1
					} else { 
						toques += 1
					}
				}
			}
		}
		return (toques, famas)
	}

Compárenla con Go:

	func comparar(num, sec []int) (toques int, famas int) {
		for i, n := range num {
			for j, m := range sec {
				if n == m {
					if i == j { 
						famas++ 
					} else { 
						toques++ 
					}
				}
			}
		}
		return
	}

O con Rust:

	fn comparar(sec:&Vec<u32>, num:&Vec<u32>) -> (usize, usize) {
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
		return (toques, famas);
	}

Todas usan el concepto de "enumerador", que nos permite obtener el elemento y la posición de un arreglo o secuencia (o iterador).

## Generación de Secuencia Aleatoria

Mirar Package.swift donde incluimos el Package FisherYates para usar el metodo shuffle.

## ¿Donde aprender Swift?

Estoy leyendo

	- The Swift Programming Language (Swift 2.1), Apple, Disponible por iTunes


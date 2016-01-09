# Toque y fama en Go

Go, el lenguaje de Google. Entre sus creadores están Rob Pike y Ken Thompson. Con esas credenciales es natural que haya tanto interés por este lenguaje.

Lo interesante de Go, en mi opinión, es lo bien estructuradas que están sus herramientas y el workflow de desarrollo.

Para compilar un programa en Go debes definir la variable GOPATH para configurar tu workspace.

Sugiero leer bien el documento How To Write Go Code (https://golang.org/doc/code.html) antes de empezar a programar en Go.

Hay muy buena documentación en el sitio de Go (https://golang.org/).

La sintaxis de Go es muy familiar y fácil de aprender.

Como no existe la función Shuffle, la tuve que implementar, vean el código:

	func shuffle(a []int) []int {
		rand.Seed(time.Now().UnixNano())
		for i := range a {
			j := rand.Intn(i+1)
			a[i], a[j] = a[j], a[i]
		}
		return a
	}

En Go las llaves {} son obligatorias en un for o un if, lo que elimina la ambiguedad conocida como "dangling-else".
Esto hace que los paréntesis desaparezcan. 
Los punto y coma son opcionales, en realidad el fin de linea (\n) reemplaza al punto y coma.

Los tipos se declaran después de las variables, por ejemplo:

	a []int 

Declara la variable a como un arreglo de enteros.

El manejo de errores se hace usando la capacidad de Go de retornar multiples resultados en una función. Esta capacidad no la estoy usando en este ejemplo y es por esta razón que el programa tiene un pequeño bug: cuando ingresas ^D en Unix el programa no termina (un requisito que como en otras implementaciones). Dejo al lector el desafío de corregir ese bug.

Para comparar la secuencia generada con el número ingresado por el jugador y contar los toques y las famas usamos el siguiente código:

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

Esta función retorna una dupla (tupla con dos elementos).
Notar que las variables toques y famas se incializan en 0 automáticamente y que podemos usarlas dentro del código, cuando se invoque return el programa devolverá estas variables a la llamada.

De este modo la puedes llamar así:

	toques, famas := comparar(num, sec)

Go tiene inferencia de tipo, así que mediante el operador := se declara e inicializan las variables.


## ¿Donde aprender Go?

Yo estoy leyendo:

	- The Go Programming Language, de Donovan y Kernighan http://amzn.to/1VROtOZ,.
	- La documentación del sitio oficial de Go es muy buena.

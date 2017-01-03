# Steely Dan y la perfección del sonido

Durante la década de 1970 y parte de la década de 1980, Donald Fagen y Walter Becker, los fundadores de Steely Dan, aparte de ser reconocidos como grandes músicos y compositores, adquirieron fama por su excesivo perfeccionismo. Se dice, pero no he encontrado evidencia, que en uno de sus álbumes pidieron excusas por la calidad de la grabación, la que no cumplía con sus altos estándares de calidad. Esta obsesión se notó, por ejemplo, en la producción del album Gaucho, para el cual terminaron trabajando con  con 42 diferentes músicos para producir el séptimo álbum de la banda. En un momento contrataron nada menos que a Mark Knopfler, lider de Dire Straits, para que aportara con un solo de guitarra. Fagen y Becker quedaron impresionados después de escuchar Sultans of Swing, e invirtieron el dinero que no tenían para contar con Kopfler en el disco, después de varias horas de grabación lo que quedó fueron unos segundos que se escuchan al inicio de la canción "Time Out of Mind".

La perfección es inalcanzable, por supuesto, pero no quita que nos obsesionemos por tratar de lograrla. Para Dijkstra, por ejemplo, la perfección en el código estaba en la belleza y elegancia del mismo. ¿Pero qué hay del desempeño del software? Quizá el programa más elegante no es el más eficiente en tiempo de ejecución.

Durante la grabación de "Katty Lied"[1], Fagen y Becker experimentaron una de las mayores frustraciones de su historia. La tecnología que eligieron para grabar, aún siendo de vanguardia, falló causando una serie de inconvenientes técnicos. Al grado que abandonaron el proceso de edición y se rehusaron a escuchar el producto final. Fue el guitarrista Denny Dias quien se encargó de terminar el proceso de edición, en una heroica jornada (documentada acá[http://steelydan.com/dennys3.html]).

Uno de los problemas que enfrentaron, después de corregir el sonido de las cintas, fue que al transferir la grabación al vinilo notaron que la calidad se deterioraba. Otro problema es que el sonido no era el mismo y dependía del reproductor. Fue que tuvieron que volver a trabajar en la mezcla para lograr un sonido que fuera reproducible en un "fonógrafo promedio". Una tarea que resultó imposible, así que terminaron conformándose con lograr que el disco fuera aceptable para la mayor cantidad de reproductores.

En el desarrollo de software nos topamos con problemas similares, nuestro programa debe ser compatible con diversos ambientes, sistemas operativos, dispositivos, tipos de CPU, etc. Cómo lograr un desempeño adecuado en cada uno de los ambientes es un desafío análogo al que tuvieron que enfrentar estos ingenieros de sonido.

# Optimizar hasta que no duela

"La optimización prematura es la raíz de todo mal", es la cita de Donald Knuth más mencionada cuando hablamos de optimizar código. En realidad la cita completa dice lo siguiente:

"No hay duda que el grial de la eficiencia lleva al abuso. Los programadores gastan una enorme cantidad de tiempo pensando, o preocupándose, de la velocidad de secciones no críticas de sus programas, y esos intentos de eficiencia en realidad tienen impactos negativos fuertes cuando consideramos la depuración y mantención. Deberíamos olvidar pequeñas optimizaciones, digamos el 97% del tiempo: la optimización prematura es la raíz de todo mal. 

Pero aún así no deberíamos dejar pasar nuestras oportunidades en ese 3% crítico. un buen programador no debe ser arullado por la complacencia ante tal razonamiento, debe ser sabio para mirar cuidadosamente al código crítico, pero sólo después que este código ha sido identificado. A menudo es un error hacer juicios a priori sobre las partes de un programa que son realmente críticas, puesto que la experiencia universal de los programadores que han usado herramientes de medición ha sido que sus intuiciones a menudo fallan." [2]

Este artículo corresponde a la tercera etapa de mi desafío de aprender 9 lenguajes de programación a través de nueve problemas. 
Si quieres saber más sobre este proyecto lee acá: http://www.lnds.net/blog/lnds/2016/1/9/esos-raros-lenguajes-nuevos

Esta vez aproveché el problema para explorar un poco más ciertas características de los lenguajes, tratando de optimizar las soluciones opara lograr el máximo de velocidad durante la ejecución.

# Un problema de la vida real

Este problema en particular está basado en una situación real que se dio en mi trabajo. Un grupo de analistas había desarrollado una serie de macros en Excel (apoyados en VB) para procesar un conjunto de archivos. El problema es que esta solución tomaba varias horas para un archivo de unas miles de lineas, y en operación real los archivos tendrían varios millones de registros, lo que hacía inviable procesarlos. No teníamos en ese momento capacidad para atender este requerimiento, así que decidí crear un pequeño utilitario en C para reemplazar las macros en Excel.

Este ejercicio reproduce más o menos el mismo problema y su enunciado es el siguiente:

	Se debe construir un filtro que reciba un archivo de vectores y los consolide en un archivo de salida.

	La invocación del programa es la siguiente:

    	$ ordenar_vector archivo_entrada archivo_salida

	Si no se le entregan argumentos al programa este debe salir con un mensaje de error.
	Al finalizar debe desplegar el tiempo, en minutos y segundos, empleado en procesar todo el archivo de entrada.

	La entrada consiste en un archivo en que cada línea se divide en:

	    Encabezado: 9 dígitos
	    Detalle: que consiste en 6 vectores
	    Vector: que contiene en 23 elementos que corresponden a periodos calendario (mes de algún año)
	    Periodo: un número de 6 dígitos, puede ser 000000 o un número de la forma AAAAMM donde AAAA es un año y MM un mes.


	La operación que se debe realizar es la siguiente:

    	1. Se deben consolidar todos los periodos de los 6 vectores en un vector de a lo más 23 elementos.
    	2. Si los periodos se repiten se debe dejar sólo 1.
    	3. Los periodos se deben ordenar de mayor a menor.
    	4. La salida debe ser la siguiente:
        	Encabezado: 9 dígitos que se copian de la entrada
        	Marca: una letra que puede tener los valores S, N ó D.
        	Vector: un vector de a lo más 23 periodos.
    	5. Se debe considerar lo siguiente:
        	5.1 Si el vector consolidado tiene más de 23 elementos se debe colocar la marca S y el vector se debe llenar de espacios en blanco.
        	5.2 Si el vector consolidado tiene cero elementos (porque vienen sólo 0s en los periodos) se debe colocar la marca N.
        	5.3 Si el vector tiene menos de 24 elementos se debe colocar la marca D.

    Hay que considerar lo siguiente:
    	1. Si una linea tiene un largo distinto al esperado se debe reportar el error indicando el número de linea (numeradas a partir de 0).
    	2. Si no es posible abrir un archivo se debe reportar el error.

Para efectos de referencia, incluí una solución en C que es muy similar al programa que implementé originalmente.

# Buscando la solución más rápida

Para efectos de prueba, ejecuté todas las soluciones usando un archivo de un millón de lineas. Hay un script en Perl que pemite generar archivos de prueba, que están incluidos en el repositorio GitHub de este proyecto.

La "diversión" de este desafío consistió en reducir el tiempo de ejecución de cada solución y tratar de superar la solución en C.

Si compilas mi solución en C sin ningún tipo de optimización, el tiempo de ejecución para un millón de lineas es de 8.8 segundos en mi notebook
[3]. Con la opción -O3 el tiempo de ejecución se reduce a 3.54 segundos.

Lo sorprendente fue encontrar una solución en Go más rápida que se ejecuta en apenas 2.92 segundos!

Este es el ranking medido en mi PC, para un millón de lineas:

	|  # | Lenguaje |   Tiempo |
	|  1 | Go       |    2.92  |
	|  2 | C Opt.   |    3.54  |
	|  3 | Kotlin   |    3.81  |
	|  4 | Rust     |    3.99  |
	|  5 | Swift    |    4.66  |
	|  6 | Scala    |    6.02  |
	|  7 | Clojure  |    7.79  |
	|  8 | Haskell  |    8.05  |
	|  9 | F#       |    8.64  |
	| 10 | Erlang   |   49.25  |

Este tipo de operaciones no es algo para lo que Erlang (y sus bibliotecas estándares) no están diseñado (aunque sospecho que es posible construir una solución más rápida usando binaries, en vez de strings, pero no conozco suficiente de Erlang para demostrarlo).

# Cómo resolver este problema

La forma general para resolver este problema se puede expresar en seudo código del siguiente modo:

	- por cada linea en el archivo de entrada:
		- si el largo de linea no es el apropiado -> imprimir error indicando el numero de linea
		- si el alrgo de linea es el que corresponde:
			- dividir la linea en 6*23 periodos (strings de tamaño 6)
			- descartar los periodos que sólo contengan ceros ("000000")
			- descartar los periodos duplicados
			- ordenar los periodos de mayor al menor
			- Finalmente escribir en la salida lo siguiente:
				- si la cantidad de periodos que quedan es 0 colocar una N y rellenar la salida con blancos
				- si la cantidad de periodos que quedan es mayor que 23 colocar una S y rellenar la salida con blancos
				- en cualquier otro caso colocar una D, luego concatenar los periodos y rellenar con blancos

# La solución más breve

En términos de cantidad líneas de código este es el ranking:

    # |  Lenguaje |   Lineas |
    1 |  F#       |     52   |
    2 |  Haskell  |     62   |   
    3 |  Kotlin   |     64   |
    4 |  Clojure  |     70   |
    5 |  Erlang   |     75   |
    6 |  Scala    |     82   |
    7 |  C        |     90   |
    8 |  Rust     |     90   |
    9 |  Go       |     94   |
   10 |  Swift    |    129   |


Nuevamente Kotlin en el tercer lugar.


# Ordenando los vectores en C, Rust, Go y Swift

En C, Rust, Go y Swift la solución se construyó de modo similar.

## C

En C creamos un área de trabajo la que ordenamos "in place" usando un algoritmo de sort de inserción. 
El área de trabajo es un arreglo de periodos (de ancho 6, definido en la macro VECTOR_ELEM_SIZE).

El código para ordenar el vector queda así:

	int ordena_vector(char* vector)
	{
		char vector_trabajo[VECTOR_SIZE*CANTIDAD_INSTITUCIONES][VECTOR_ELEM_SIZE];
		int i, j;
		int n = 0;
		char* p = vector;
		char* vend = vector+(VECTOR_SIZE*VECTOR_ELEM_SIZE);
		memset((char*)vector_trabajo, '0', VECTOR_SIZE*CANTIDAD_INSTITUCIONES*VECTOR_ELEM_SIZE);
		
		// por cada elemento del vector
		for (p = vector; p < vend && *p != ' '; p+= VECTOR_ELEM_SIZE)
		{
			// sort por insercion
			for (i = 0; i < n && strncmp(p, vector_trabajo[i], VECTOR_ELEM_SIZE) < 0; i++)
				;
	        if (i == n) {
	    		if (strncmp(p, vector_trabajo[n], VECTOR_ELEM_SIZE) != 0)
	  		 		memmove(vector_trabajo[n++], p, VECTOR_ELEM_SIZE);
		  	}
		    else {			
		    	if (strncmp(p, vector_trabajo[i], VECTOR_ELEM_SIZE) != 0) {
		  			for (j = VECTOR_SIZE-1; j > i; j--)
		  				memmove((char*)vector_trabajo[j], vector_trabajo[j-1], VECTOR_ELEM_SIZE);
		  			memmove(vector_trabajo[i], p, VECTOR_ELEM_SIZE);
		  			n++;	
		  		}
		  	}
	  	
		} // for
		
		memset(vector, ' ', VECTOR_ELEM_SIZE*VECTOR_SIZE+1);
		for (i = 0, p = vector+1; i < n; i++, p+= VECTOR_ELEM_SIZE)
			memmove(p, (char*)vector_trabajo[i], VECTOR_ELEM_SIZE);
		return n;
	}

Este función retorna la cantidad de elementos que quedan en el vector. Acá vector es el string leido.

Una optimización obvia sería usar el mismo buffer de entrada com área de trabajo y ordenarlo. Ese es un bonito desafío en si mismo que queda propuesto.
Otra cosa, este código, al momento de escribir esto, tiene un bug, te desafío a indicar cuál es.

## Rust

Para la solución Rust imité lo que hace la solución en C.


	fn ordenar_vector(vector:&[u8],  result:&mut [u8]) {
		let mut n = 0;
		let mut trabajo = ['0' as u8; TAM_VECTOR_ENTRADA];

		for p in vector.chunks(TAM_PERIODO) {

			if p == CERO { continue; }

			let mut i = 0;
			let mut q = 0;
			while i < n && p < &trabajo[q..q+TAM_PERIODO] { i += 1; q += TAM_PERIODO; } // busca si p está en el arreglo

			if i < n && p == &trabajo[q..q+TAM_PERIODO] { continue; } // si ya existe lo ignora

			// inserta p en el arreglo
			if i == n {
				let q = n * TAM_PERIODO;
				&trabajo[q..q+TAM_PERIODO].clone_from_slice(p);
			} else {
				for j in (i+1..ELEMENTOS_VECTOR).rev() {
					let q = j*TAM_PERIODO;
					unsafe {
						ptr::copy_nonoverlapping(&mut trabajo[q-TAM_PERIODO], &mut trabajo[q], TAM_PERIODO)
					}
				}
				let q = i*TAM_PERIODO;
				trabajo[q..q+TAM_PERIODO].clone_from_slice(p);
			}
			n += 1;
		}

		// retorna el resultado
		if n == 0 {
			result[0] = 'N' as u8;
		} else if n > ELEMENTOS_VECTOR {
			result[0] = 'S' as u8;
		} else {
			result[0] = 'D' as u8;
			for i in 0..n {
				let p = i*TAM_PERIODO;
				result[p+1..p+1+TAM_PERIODO].clone_from_slice(&trabajo[p..p+TAM_PERIODO])
			}
		}
	}

El tipo de datos u8 corresponde a un byte sin signo. En Rust los vectores tienen un método bastante conveniente llamado chunks(n), que permite dividir el vector en subvectores de tamaño n. En este loop la variable p es cada periodo del vector (uno de los chunks).

El equivalente a memove() en C se logra en Rust usando rangos.
Por ejemplo, la expresión:
	
	&trabajo[q..q+TAM_PERIODO].clone_from_slice(p);

Lo que hace es copiar en ese segmento del arreglo de trabajo. 

Hay que recordar que Rust tiene unas estrictas reglas para copiar datos de un area de memoria a otra y las reglas de ownership obligan a "clonar" bytes de un arreglo a otro[4].

Es por esta razón que para mover dentro de un arreglo debemos usar código unsafe:

	unsafe {
		ptr::copy_nonoverlapping(&mut trabajo[q-TAM_PERIODO], &mut trabajo[q], TAM_PERIODO)
	}

También se podría optimzar más el código haciendo un sort in place en sólo un buffer, lo que queda propuesto.

## Go

En Go la solución es igual de sencilla y similar a las versiones en Rust y C

	func ordenar_vector(buf []byte, result []byte) {
		n := 0	
		trabajo := make([]byte, TAM_VECTOR_ENTRADA, TAM_VECTOR_ENTRADA)
		for i := 0; i < TAM_PERIODO; i++ { cero[i] = '0' }
		for i := 0; i < TAM_VECTOR_ENTRADA; i ++ { trabajo[i] = '0' }
		for p := 0; p < TAM_VECTOR_ENTRADA; p += TAM_PERIODO {
			periodo := buf[p:p+TAM_PERIODO]
			if bytes.Equal(periodo, cero) { continue }
			i := 0
			q := 0
			for i < n && bytes.Compare(periodo, trabajo[q:q+TAM_PERIODO]) < 0 {
				i++
				q += TAM_PERIODO 
			}

			if i < n && bytes.Equal(periodo, trabajo[q:q+TAM_PERIODO]) { continue }

			if i == n {
				q := n*TAM_PERIODO
				copy(trabajo[q:q+TAM_PERIODO], periodo)
			} else  {
				for j := ELEMENTOS_VECTOR-1; j > i; j-- { 
					q := j*TAM_PERIODO
					copy(trabajo[q:q+TAM_PERIODO], trabajo[q-TAM_PERIODO:q])
				}
				q := i*TAM_PERIODO
				copy(trabajo[q:q+TAM_PERIODO], periodo)
			}
			n++
		}
		if n == 0 {
			result[0] = 'N'
		} else if n > ELEMENTOS_VECTOR {
			result[0] = 'S'
		} else {
			result[0] = 'D'
			copy(result[1:n*TAM_PERIODO+1], trabajo[0:n*TAM_PERIODO])
		}
	}

Usamos rangos para operar con secciones del arreglo de entrada. Copiar segmentos del vector al área de trabajo es bastante simple:
	
		copy(trabajo[q:q+TAM_PERIODO], periodo)

Los rangos, tanto en Rust como en Go van desde el indice inicial hasta el valor anterior del indice final, por ejemplo, vector[0..6] devuelve 6 elementos, desde el 0 al 5 inclusive.

## Swift

El código en Swift recibe los dos vectores que son declarados externamente:

	func ordenarVector(_ buf: [Int8],  _ trabajo : inout [Int8]) -> Int {
		var p = posVector
		var n = 0
		let tope = largoLinea-1
		while p < tope {
			if buf[p..<p+tamPeriodo] == ceroData {
				p += tamPeriodo
				continue
			}
			var i = 0
			var q = 0
			while i < n && buf[p..<p+tamPeriodo].lexicographicallyPrecedes(trabajo[q..<q+tamPeriodo]) {
				i += 1
				q += tamPeriodo
			}

			if i < n && buf[p..<p+tamPeriodo] == trabajo[q..<q+tamPeriodo] {
				p += tamPeriodo
				continue
			}

			if i == n {
				q = n * tamPeriodo
				for k in 0..<tamPeriodo {
					trabajo[q+k] = buf[p+k]
				}
			} else {
				var j = tamVector-1
				while j > i {
					q = j * tamPeriodo
					for k in 0..<tamPeriodo {
						trabajo[q+k] = trabajo[q-tamPeriodo+k]
					}
					j -= 1
				}
				q = i * tamPeriodo
				for j in 0..<tamPeriodo {
					trabajo[q+j] = buf[p+j]
				}
			}
			n += 1
			p += tamPeriodo
		}
		return n
	}

Swift tiene operaciones para copiar segmentos (slices) de un arreglo, uno puede escribir lo siguiente:

	trabajo[q..<q+tamPeriodo] = bug[p..<p+tamPeriodo]

Pero noté que el compilador genera código muy ineficiente para estas operaciones, pues genera un objeto para cada segmento que luego es liberado. Es por esto que opté por escribir estos loops para copiar los elementos de un vector:

	for j in 0..<tamPeriodo {
					trabajo[q+j] = buf[p+j]
				}

Espero que futuras versiones del compilador de Swift resuelvan este problema y genern código más eficiente.

# Solucionando con la JVM

Hay tres lenguajes basados en la JVM en este desafío, Clojure, Scala y Kotlin.

## Clojure

Mi primera solución fue en Clojure, que es un lenguaje dinámico funcional. El código para ordenar el periodo es bastante sencillo:

	(defn agregar-periodo [^String linea ini fin lista]
		(if (.regionMatches linea ini ceros 0 tam-periodo)
			lista 
			(conj! lista (subs linea ini fin))))

	; lista debe ser un set
	(defn extraer-periodos [^String linea]
		(loop [ini pos-vector fin pos-segundo-periodo lista (transient #{})]
			(if (= ini tope-linea) (persistent! (agregar-periodo linea ini fin lista))
			(recur (+ ini tam-periodo) (+ fin tam-periodo) (agregar-periodo  linea ini fin lista)))))


	(defn ordenar-periodos [^String linea]
		(let [periodos  (extraer-periodos linea)
			  n (count periodos)]
			 (cond 
				(zero? n) (str "N" relleno-vector)
			 	(> n elementos) (str "S" relleno-vector)
			 	:else (str "D" (s/join (take elementos (concat (sort #(compare ^String %2 ^String %1) periodos) (repeat relleno))))))))

Lo primero que hay que notar es que ordenamos el vecctor sólo al final, después de que hemos determinado que la cantidad de periodos es mayor que cero y menor igual que 23. 

La función extraer periodos va insertando cada periodo en un set, lo que permite eliminar duplicados. Por cierto, sólo agregamos el periodo si este es distinto a cero.

Notar que usamos un set creado con la función (transient), que nos permite un mayor desempeño.

Pero la verdadera optimización vino cuando usé la función (.regionMatches), esta es una función de la clase String de Java. El uso de esta función permite evitar la creación de substrings, lo que influye en el desempeño final de esta solución.

La función regionMatches() permite comparar una región del string, esto nos permite saber si la subsección del string es cero de manera bastante rápida.

Esta función fue usada posteriormente en Kotlin y Scala.

## Scala

El ordenamiento de Scala usa un conjunto ordenado, con esto el programa mantiene un arreglo sin duplicados y ordenado desde el principio.

  def ordenarPeriodos(linea: String) : String = {
    val encabezado = linea.slice(0, posVector)
    val myOrdering = Ordering.fromLessThan[String](_ > _)
    var periodos = SortedSet.empty[String](myOrdering)
    var pos = posVector
    while (pos < largoLinea) {
      if (!linea.regionMatches(pos, ceros, 0, tamPeriodo)) 
         periodos += linea.slice(pos, pos+tamPeriodo)
      pos += tamPeriodo
    }

    
    val len = periodos.size
    encabezado + (
      if (len == 0) 
          "N" + (" " * tamRelleno)
      else if (len > tamVector)
          "S" + " " * tamRelleno
      else 
          "D" + periodos.mkString + " "*(tamRelleno-len*tamPeriodo)
    )
  }

Notar como usamos regionMatches() para evitar insertar los ceros.

## Kotlin

La solución en Kotlin es la más rápida en la JVM y quizás la explicación sea que el ordenamiento lo hacemos al final, por lo que es más rápido que la solución en Scala, que está insertando en una estructura que se mantiene ordenada. La optimización en Scala es obvia, y queda propuesta como ejercició.

	fun ordenarVector(linea:String) : String {
	    val encabezado = linea.substring(0, posVector)
	    val periodos = HashSet<String>()
	    for (i in posVector until largoLinea step tamPeriodo) {
	        if (!linea.regionMatches(i, ceros, 0, tamPeriodo, true))
	            periodos.add(linea.substring(i, i+tamPeriodo))
	    }
	    if (periodos.size == 0)
	        return encabezado+"N"+ relleno
	    else if (periodos.size > tamVector)
	        return encabezado+"S"+relleno
	    else {

	        return encabezado+"D"+(periodos.sortedDescending().joinToString("")).padEnd(tamVector* tamPeriodo)
	    }
	}

# Tres lenguajes funcionales más

Las soluciones que quedan están en lenguajes funcionales, Haskell, F# y Erlang.


## Haskell

La solución es Haskell debe leerse de abajo para arriba para entenderse:


	chunksOf :: Int64 -> LB.ByteString -> [LB.ByteString]
	chunksOf x xs = unfoldr (justWhen not_null (L.splitAt x)) xs 

	periodo_valido :: LB.ByteString -> Bool
	periodo_valido xs = LB.any (/= '0') xs

	clasificar_resultado :: [LB.ByteString] -> [LB.ByteString]
	clasificar_resultado xs 
	    | null xs = ["N"]
	    | (length xs) > elementos' = ["S"]
	    | otherwise = "D" :  take elementos' xs

	ordenar_periodos :: [LB.ByteString] -> [LB.ByteString] 
	ordenar_periodos xs =  sortDesc $ nub $ (filter periodo_valido xs)


La llamada a estas funciones es así:

	clasificar_resultado $ ordenar_periodos $ chunksOf tam_periodo resto

Lo que hacemos es dividir los periodos en segmentos de tamaño 6 (tam_periodo):

	chunksOf tam_periodo resto

Luego ordenar_periodos ordena en orden descendente, eliminando duplicados (nub), filtrando sólo los periodos válidos.

Una optimización obvia sería ordenar sólo en el caso "D", lo que también es un ejercicio que pueden probar ustedes.

## F#

El ordenamiento de los periodos es bastante simple:

	let ordenar_periodos (linea:string) = 
	    let periodos = separar_periodos linea |> Seq.distinct |> Seq.toList  

	    let len = Seq.length periodos
	    if len = 0 then "N".PadRight(PAD_SIZE)
	    else if len > ELEMENTOS_VECTOR then "S".PadRight(PAD_SIZE)
	    else ("D" + (periodos |> Seq.sortDescending |> String.Concat)).PadRight(PAD_SIZE)

La función separar_periodos fue programada de manera imperativa para poder lograr un mejor performance, esto permitió reducir el tiempo varios segundos:

	let separar_periodos (linea:string) = seq {
	    let mutable p = POS_VECTOR
	    while p < LARGO_LINEA do
	        if no_es_cero linea p then
	            yield linea.Substring(p, TAM_PERIODO)
	        p <- p + TAM_PERIODO
	}
    
Esta implementación de F# pudo haber sido escrita de forma más compacta, pero tuve que recurrir a varias optimizaciones usando código más imperativo. La primera solución en F# se ejecutaba en más de 20 segundos. Con estas optimizaciones llegamos a menos de 9 segundos.

# Erlang

La solución en erlang es recursiva, y usa una estructura de arreglos ordenados. Esto fue lo más rápido que pude lograr, con tiempos menores a 50 segundos. La versión inicial tomaba más de tres minutos para un archivo de un millón de lineas, uno de los impactos más grandes fue usar archivos en modo raw, puesto que Erlang usa otro proceso para ejecutar todo el IO a menos que los archivos sean abiertos en modo raw. Después de eso las mejoras fueron marginales. Quizás con un equivalente a la función regionMatches de JVM esta versión podría bajar de los 20 segundos. Sospecho que usando binaries también, pero eso requiere conocimientos más avanzados de Erlang.

	ordenar_vector(Vector) ->
		Encabezado = substr(Vector, 1, ?POS_VECTOR),
		Periodos = separar_periodos(substr(Vector, ?INI_VECTOR, ?LARGO_VECTOR), new(), ?LARGO_VECTOR),
		Largo = size(Periodos),
		if Largo =:= 0 -> [Encabezado|?N_RELLENO];
		   Largo > ?ELEMENTOS_VECTOR -> [Encabezado|?S_RELLENO];
		   true ->  P = reverse(to_list(Periodos)),
		   			L = (?TAM_RELLENO-len(P)*?TAM_PERIODO) - 1, 
		   			[Encabezado, "D", P, chars(32, L)]
		end.

	separar_periodos(Linea, Periodos, ?TAM_PERIODO) -> 
		if Linea =:= ?CEROS -> Periodos;
		   true -> add_element(Linea, Periodos)
		end;

	separar_periodos(Linea, Periodos, Largo) ->
		Periodo = substr(Linea, 1, ?TAM_PERIODO),
		Resto = substr(Linea, ?TAM_PERIODO_MAS_1),
		if Periodo =:= ?CEROS -> separar_periodos(Resto, Periodos, Largo-?TAM_PERIODO);
		   true -> separar_periodos(Resto, add_element(Periodo, Periodos),  Largo-?TAM_PERIODO)
		end.


Este fue un ejercicio bastante largo, tomándome unas 48 horas de trabajo a lo largo de cuatro meses, la razón es que me empeñé en lograr la solución más rápida de cada lenguaje. La vara contra la que me medí fue lograr tiempos de ejecución menores a los 10 segundos. En retrospectiva puedo ver que es posible mejorar aún más varias de las soluciones. Ya quiero avanzar hacia otras cosas, así que ese desafío queda para ustedes, siempre pueden hacer un pull request con mejores soluciones o proponer soluciones en otros lenguajes.

El repositorio en Github se encuentra en: https://github.com/lnds/9d9l


Notas


[1] Katty Lied es el cuarto álbum de estudio de Steely Dan. Entre otras cosas se destaca por contar con la participación por primera vez de Michael McDonald en los coros y del gran baterista, y fundador de Toto, Jeff porcaro, que en ese tiempo tenía apenas 20 años de edad.

[2] Structured Programming with Goto Statements, hay una copia del artículo acá: http://web.archive.org/web/20130731202547/http://pplab.snu.ac.kr/courses/adv_pl05/papers/p261-knuth.pdf

[3] Macbook pro 2016, Intel i7, 2.6 Ghz, 16 Gb RAM, MacOS Sierra, Disco SSD. Las pruebas se ejecutaron sin tener ningún otro proceso activo, desconectados de internet y con anti virus deshabilitado

[4] Ver https://doc.rust-lang.org/book/ownership.html



# Compresión

Este desafio consiste en construir una aplicación que comprima archivos, usando la codificación de Huffman.

La invocación del programa es la siguiente:

	$ huffman [-c|-d] archivo_entrada archivo_salida

Si se invoca con -c comprime el archivo_entrada y lo deja en el archivo_salida. 
Si se invoca con -d recibe un archivo_entrada previamente comprimido y lo deja en archivo_salida.

El objetivo de este desafío es construir el programa tratando de no usar estructuras de datos contenidas en las bibliotecas de los programas. 

Para implementar la codificación de huffman requerimos colas de prioridad y árboles binarios, la idea es constuirlos.

Esto lo hacemos para aprender mucho más del lenguaje y ver qué nos aportan cuando tratamos de programar nuestras propias bibliotecas.

# El algoritmo

Para comprimir:

	1. Contamos la frecuencia de caracteres y lo llevamos a una tabla con dos campos: (carácter, frecuencia).
	2. Por cada tupla de la tabla anterior creamos una hoja, cuya etiqueta es la frecuencia y el valor es el caracter representado.
	3. Insertamos las hojas en una cola de prioridad mínima.
	4. Tomamos los dos primeros elementos de la cola y creamos un nodo que contiene las dos hojas y la etiquetamos con la suma de las dos etiquetas. Además etiquetamos las ramas con 0 para la rama izquierda, 1 para la rama derecha. Insertamos el nodo en la cola de prioridad.
	5. Repetimos 4 hasta que quede sólo 1 nodo en la cola.

	Con esto tenemos un árbol de Huffman.

	Luego por cada caracter obtenemos su código del siguiente modo:

	1. Comenzar con un código vacío
	2. Iniciar el recorrido del árbol en la hoja asociada al caracter
	3. Comenzar un recorrido del árbol hacia arriba
	4. Cada vez que se suba un nivel, añadir al código la etiqueta de la rama que se ha recorrido.
	5. Tras llegar a la raíz, invertir el código. 
	6. El resultado es el código Huffman deseado


Para descomprimir se debe tener la tabla de códigos de huffman usada para comprimir. Con esto se va leyendo cada bit de la entrada y se recorre el árbol, al llegar a la hoja con el caracter se emite el caracter.

# Teoría

El algoritmo de Huffman se basa en el concepto de entropía de la información de Shannon y en teoría permite codificar de la forma más eficiente un texto, puesto que se ajusta a las frecuencias reales de los símbolos que lo componen. En la práctica hay algoritmos que comprimen mucho más, pues pueden agrupar repeticiones.

Otro problema con esta codificación es que requiere incluir la tabla de frecuencias, o el árbol en el archivo de salida. 
Una alternativa es tener una tabla de frecuencias fija, pero esta no necesariamente se ajusta a todos los archivos.
Hay que considerar que si todos los símbolo tienen la misma frecuencia, el código de Huffman degenera en la codificación binaria tradicional y no hay compresión.

Para aprender más sobre el algoritmo leer: https://en.wikipedia.org/wiki/Huffman_coding


# Resultados

## Tiempos

Tiempos promedios, expresados en segundos, para procesar un archivo de tamaño XX Mb.

    | Lenguaje | Tiempo | 
    | Kotlin   |        |
    | Scala	   |        |
    | Clojure  |        |

## Lineas de código

Calculadas usando la herramienta cloc (https://github.com/AlDanial/cloc)

	| Lenguaje |  LOC | LOC2 |
    | Kotlin   |  267 |  266 |
    | Scala    |  280 |  226 |
    | Clojure  |  152 |   na |
    | Rust     |  336 |      |
    | Go	   |  338 |   na |
    | Swift    |  334 |   na |
    | F#       |  197 |   na |
    | Erlang   |   69 |   na |
    | Haskell  |   91 |   na |

## Tiempo de Desarrollo

Tiempos aproximados para desarrollar cada solución, medidos en horas:minutos, considera codificación, pruebas e investigación.
Para medir estos tiempos usé la herramienta TimingApp para Mac OSX (http://TimingApp.com/)

    | Kotlin  | 5:01 |
    | Scala   | 3:46 |
    | Clojure | 4:48 |
    | Rust    | 6:56 |
    | Go	  | 3:15 |
    | Swift   | 3:18 |
    | F#	  | 4:28 |
    | Erlang  | 4:25 |
    | Haskell | 7:33 |

## Ranking

Ordenado del más rápido en ejecución al más lento (medidos usando el comando time):

    |  # | lenguaje     | Tiempo Compresión | Tiempo Descompresión | Total
    |  1 | Rust         | 0.035s            | 0.050s               |  0.085s
    |  2 | Go           | 0.089s            | 0.141s               |  0.230s
    |  3 | Kotlin2      | 0.200s            | 0.212s               |  0.412s
    |  4 | Kotlin       | 0.199s            | 0.224s               |  0.423s
    |  5 | Scala        | 0.493s            | 0.463s               |  0.956s
    |  6 | Scala2       | 0.529s            | 0.468s               |  0.997s
    |  7 | Swift        | 0.723s            | 1.011s               |  1.734s
    |  8 | Haskell      | 2.384s            | 0.809s               |  3.193s
    |  9 | Erlang       | 2.069s            | 2.156s               |  4.225s
    | 10 | Clojure      | 6.973s            | 4.960s               | 11.933s
    | 11 | F#           | 3.942s            | 8.938s               | 12.880s
    

Ordenados del más corto en lineas de código, al más largo:

    # | Lenguaje | Lineas |
    1 | Erlang   |     69 |
    2 | Haskell  |     91 |
    3 | Clojure  |    152 |
    4 | F#       |    197 |
    5 | Kotlin   |    267 |
    6 | Scala    |    280 |
    7 | Swift    |    334 |
    8 | Rust     |    336 |
    9 | Go       |    338 |
 

## Aportes

 

# Licencia

	(c) 2016, 2017 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.

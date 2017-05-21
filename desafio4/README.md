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

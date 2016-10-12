# ordenar_vector en Go

## Archivos

Abrir un archivo de entrada:

	entrada, err := os.Open(os.Args[1])
	if err != nil {
		panic(fmt.Sprintf("No pudo abrir archivo %s, causa: %s\n", os.Args[1], err))
	}
	defer entrada.Close()

Go retorna un error si hay problemas al abrir un archivo.
Notar la sentencia defer entrada.Close() esto asegura que el archivo se cierra al retornar de la función.

Crear archivo de salida:

	salida, err := os.Create(os.Args[2])
	if err != nil {
		panic(fmt.Sprintf("No pudo crear archivo %s, causa: %s\n", os.Args[2], err))
	}
	defer salida.Close()

Notar lo mismo.

PAra leer las lineas del archivo usamos el paquete bufio que soporte Buffers:

	lector := bufio.NewScanner(entrada)
	writer  := bufio.NewWriter(salida)
	defer writer.Flush()

Acá es importante notar que el buferr de escritura tienen un defer writer.Flush().
Inicialmente no tenía esta línea de código y el archivo quedaba incompleto, lo que me tomó bastante tiempo de determinar.

La lectura de todas las lineas del archivo se hace así:


	for nl := 0; lector.Scan(); nl++ {
		linea := lector.Bytes()
		if len(linea) != LARGO_LINEA {
			fmt.Printf("!!! Largo incorrecto en linea: %d\n", nl)
		}
		vector := procesar_linea(linea)
		writer.WriteString(fmt.Sprintf("%s\n", vector))
	}

Notar que linea es un arreglo con los bytes de la linea, que se pasa a la función procesar_linea().


## Slices y Arreglos de Bytes

La función procesar_linea() es la siguiente:

	func procesar_linea(buf []byte) []byte {
		result := make([]byte, TAM_SALIDA, TAM_SALIDA) 
		i := 0
		for ; i < POS_VECTOR; i++ {
			result[i] = buf[i]
		}
		for ; i < TAM_SALIDA; i++ {
			result[i] = ' '
		}
		ordenar_vector(buf[POS_VECTOR:], result[POS_VECTOR:])
		return result 
	}


Creamos un buffer de salida usando la primitiva make().

Notar que lo inicializamos con espacios en blanco. Luego llamamos a la función ordenar_vector que trabaja con el vector result[] in place.os usado un puntero.

## El ordenamiento

El código del ordenamiento es el siguiente:

	func ordenar_vector(buf []byte, result []byte) {
	n := 0	
	cero := make([]byte, TAM_PERIODO, TAM_PERIODO)
	trabajo := make([]byte, TAM_VECTOR_ENTRADA, TAM_VECTOR_ENTRADA)
	for i := 0; i < TAM_PERIODO; i++ { cero[i] = '0' }
	for i := 0; i < TAM_VECTOR_ENTRADA; i ++ { trabajo[i] = '0' }
	for p := 0; p < TAM_VECTOR_ENTRADA; p += TAM_PERIODO {

		if bytes.Equal(buf[p:p+TAM_PERIODO], cero) { continue }
		i := 0
		q := 0
		for i < n && bytes.Compare(buf[p:p+TAM_PERIODO], trabajo[q:q+TAM_PERIODO]) < 0 {
			i++
			q += TAM_PERIODO 
		}

		if i < n && bytes.Equal(buf[p:p+TAM_PERIODO], trabajo[q:q+TAM_PERIODO]) { continue }

		if i == n {
			q := n*TAM_PERIODO
			copy(trabajo[q:q+TAM_PERIODO], buf[p:p+TAM_PERIODO])
		} else  {
			for j := ELEMENTOS_VECTOR-1; j > i; j-- { 
				q := j*TAM_PERIODO
				copy(trabajo[q:q+TAM_PERIODO], trabajo[q-TAM_PERIODO:q])
			}
			q := i*TAM_PERIODO
			copy(trabajo[q:q+TAM_PERIODO], buf[p:p+TAM_PERIODO])
		}
		n++
	}
	if n == 0 {
		result[0] = 'N'
	} else if n > ELEMENTOS_VECTOR {
		result[0] = 'S'
	} else {
		result[0] = 'D'
		for i := 0; i < n; i++ {
			p := i*TAM_PERIODO
			copy(result[p+1:p+1+TAM_PERIODO], trabajo[p:p+TAM_PERIODO])
		}
	}
}

Es un loop bastante complejo, puesto que trata de ser lo más eficiente posible, saltando los periodos en cero.
La primitiva copy() de Go permite copiar bytes dentro del arreglo.


Problema propuesto: se puede optimizar más este loop? Hay una manera más eficiente de lograr lo mismo?



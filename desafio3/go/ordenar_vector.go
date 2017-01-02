package main

import ("bufio"; "fmt"; "os"; "time"; "bytes")

const POS_VECTOR = 9
const ELEMENTOS_VECTOR = 23
const TAM_PERIODO = 6
const CANT_INSTITUCIONES = 6
const TAM_VECTOR = ELEMENTOS_VECTOR * TAM_PERIODO
const TAM_VECTOR_ENTRADA = TAM_VECTOR * CANT_INSTITUCIONES
const LARGO_LINEA = POS_VECTOR + TAM_VECTOR_ENTRADA
const TAM_SALIDA = POS_VECTOR + 1 + TAM_VECTOR

var cero = make([]byte, TAM_PERIODO, TAM_PERIODO)


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

func main() {

	if len(os.Args) != 3 {
		fmt.Println("Uso: ordenar_vector archivo_entrada archivo_salida")
		os.Exit(0)
	}

	start := time.Now()
		
	entrada, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Printf("ERROR: No pudo abrir archivo %s, causa: %s\n", os.Args[1], err)
		os.Exit(-1)
	}
	defer entrada.Close()

	salida, err := os.Create(os.Args[2])
	if err != nil {
		fmt.Printf("ERROR: No pudo crear archivo %s, causa: %s\n", os.Args[2], err)
		os.Exit(-1)
	}
	defer salida.Close()

	lector := bufio.NewScanner(entrada)
	writer  := bufio.NewWriter(salida)
	defer writer.Flush()

	for nl := 0; lector.Scan(); nl++ {
		linea := lector.Bytes()
		if len(linea) != LARGO_LINEA {
			fmt.Printf("!!! Largo incorrecto en linea: %d\n", nl)
			writer.WriteString(fmt.Sprintf("%s\n", linea))
		} else {
			vector := procesar_linea(linea)
			writer.WriteString(fmt.Sprintf("%s\n", vector))
		}
	}
	duration := time.Since(start)
	fmt.Printf("Tiempo ocupado: %02.3f\n", duration.Seconds())
}

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
var	periodo = make([]byte, TAM_PERIODO, TAM_PERIODO)

const BUF_SIZE = 1024*1024*10
var buffer = make([]byte, BUF_SIZE)


func ordenar_vector(buf []byte) int {
	n := 0	
	for p := 0; p < TAM_VECTOR_ENTRADA; p += TAM_PERIODO {
		copy(periodo, buf[p:p+TAM_PERIODO])
		if bytes.Equal(periodo, cero) { 
			continue 
		}

		i := 0
		q := 0
		for ;i < n && bytes.Compare(periodo, buf[q:q+TAM_PERIODO]) < 0; i++ {
			q += TAM_PERIODO 
		}

		if i == n {
			if p == q {
				n++
			} else if bytes.Compare(periodo, buf[q:q+TAM_PERIODO]) != 0 {
				copy(buf[q:q+TAM_PERIODO], periodo)
				n++
			}
		} else if bytes.Compare(periodo, buf[q:q+TAM_PERIODO]) != 0 {
			l := (n-i)*TAM_PERIODO
			copy(buf[q+TAM_PERIODO:q+TAM_PERIODO+l], buf[q:q+l])
			copy(buf[q:q+TAM_PERIODO], periodo)
			n++
		}
	}
	copy(buf[1:n*TAM_PERIODO+1], buf[0:n*TAM_PERIODO])
	return n
	
}

func procesar_linea(buf []byte) []byte {
	n := ordenar_vector(buf[POS_VECTOR:])
	ini := POS_VECTOR+1
	largo := POS_VECTOR+1+TAM_VECTOR
	if n == 0 {
		buf[POS_VECTOR] = 'N'
	} else if n > ELEMENTOS_VECTOR {
		buf[POS_VECTOR] = 'S'
	} else {
		buf[POS_VECTOR] = 'D'
		ini = POS_VECTOR+1+n*TAM_PERIODO
	}
	for i := ini; i < largo; i++ {
		buf[i] = ' '
	}
	return buf[0:POS_VECTOR+1+TAM_VECTOR] 
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
	lector.Buffer(buffer, BUF_SIZE)
	writer  := bufio.NewWriter(salida)
	defer writer.Flush()

	for i := 0; i < TAM_PERIODO; i++ { cero[i] = '0' }
	for nl := 0; lector.Scan(); nl++ {
		linea := lector.Bytes()
		if len(linea) != LARGO_LINEA {
			fmt.Printf("!!! Largo incorrecto en linea: %d\n", nl)
			writer.WriteString(fmt.Sprintf("%s\n", linea))
		} else {
			vector := procesar_linea(linea)
			writer.Write(vector)
			writer.WriteByte('\n')
		}
	}
	duration := time.Since(start)
	fmt.Printf("Tiempo ocupado: %02.3f\n", duration.Seconds())
}

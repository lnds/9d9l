import Foundation

let tamVector   = 23
let posVector   = 9
let tamPeriodo  = 6
let nInsts      = 6
let largoLinea  = posVector + nInsts * tamVector * tamPeriodo + 1
let tamSalida   = posVector + 1 + tamVector * tamPeriodo + 1
let tamVecEntradaBytes = nInsts * (tamVector * tamPeriodo)
let tamVecSalidaBytes  = tamVector * tamPeriodo
let N : Int8 = 78 // letra N
let S : Int8 = 83 // letra S
let D : Int8 = 68 // letra D
let NL : Int8 = 10 // newline
let CERO : Int8 = 48 // caracter '0'
let SPACE : Int8 = 32


var ceroData = ArraySlice<Int8>(repeating: CERO, count: tamPeriodo)


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


var trabajo = [Int8](repeating: CERO, count: tamVecEntradaBytes)
var result = [Int8](repeating: SPACE, count: tamSalida+1)
		

func procesarLinea(_ buf: [Int8], _ nl: Int) -> [Int8] {
	if strlen(buf) != UInt(largoLinea) {
		print("!!! Largo incorrecto en linea \(nl) \(largoLinea) != \(buf.count)")
		return buf
	} else {
		for i in 0..<tamVecEntradaBytes {
            trabajo[i] = CERO
        }
        for i in 0..<tamSalida {
            result[i] = SPACE
        }
		let tam = ordenarVector(buf, &trabajo)
		result[0..<posVector] = buf[0..<posVector]
		if tam == 0 {
			result[posVector] = N
		} else if tam > tamVector {
			result[posVector] = S
		} else {
			result[posVector] = D
			for j in 0..<tam*tamPeriodo {
				result[j+posVector+1] = trabajo[j]
			}
		}
		result[tamSalida-1] = NL
		result[tamSalida] = 0
		return result
	}
}


let args = ProcessInfo.processInfo.arguments
let argc = ProcessInfo.processInfo.arguments.count

if argc != 3 {
	print("Uso: ordenar_vector archivo_entrada archivo_salida")
	exit(-1)
} else {
	let start = Date()
	let entrada = args[1]
	let salida  = args[2]
	let fentrada = fopen(entrada, "rt")
	if fentrada == nil {
		print("no pudo abrir archivo entrada: \(entrada)")
		exit(-1)
	}
	let fsalida = fopen(salida, "wt")
	if fsalida == nil {
		print("no pudo abrir archivo salida: \(salida)")
		exit(-1)
	}

	let BUFFER_SIZE : Int32 = 4096
	var buf = Array<Int8>(repeating: 0, count: Int(BUFFER_SIZE))
	var nl = 0 // numero de linea
	while (fgets(&buf, BUFFER_SIZE, fentrada) != nil) {
		let bufOut = procesarLinea(buf, nl)
		fputs(bufOut, fsalida)
		nl += 1
	}
	fclose(fentrada)
	fclose(fsalida)
	
	let end = Date()
	let timeInterval = end.timeIntervalSince(start)
	let secs = timeInterval.truncatingRemainder(dividingBy:3600.0)
	print(String(format:"tiempo ocupado: %05.2f",  secs))
}

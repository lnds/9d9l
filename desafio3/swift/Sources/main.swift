import Foundation


let args = ProcessInfo.processInfo.arguments
let argc = ProcessInfo.processInfo.arguments.count

if argc != 2 {
	print("Uso: ordenar_vector archivo_entrada archivo_salida")
	exit(-1)
}

# Ordenar Vector en Swift


## Archivos

El manejo de archivos de Swift es decepcionante en este momento, es decir, no existe algo como un Stream, hay un objeto FileHandle, pero está muy orientado a la manipulación de recursos, para aplicaciones GUI o móviles. 
Claramente la biblioteca estándar de Swift no está diseñada para programación de sistema.
Pero se puede invocar a las funciones de C stdlib, así que la lectura de archivos la hicimos igual que en C.


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

(Esto tiene todos los problemas de la implementación en C).

Se puede implementar una lectura más segura, hay algunas clases en Github que implementan una suerte de StreamReader y StreamWriter pero son muuuuy lentos en ejecución.

## Slices y Arreglos de Bytes

No pude definir caracteres como constantes bytes en Swift, así que tuve que hacer la cosa más fea posible:

    let N : Int8 = 78 // letra N
    let S : Int8 = 83 // letra S
    let D : Int8 = 68 // letra D
    let NL : Int8 = 10 // newline
    let CERO : Int8 = 48 // caracter '0'
    let SPACE : Int8 = 32

Si alguien sabe como declarar N como N = 'N' me avisa.

El programa emula la versión en Go, el primer intento usaba la clase Data para poder manipular memoria

    func ordenarVector(_ buf: Data) -> (tam:Int, data:Data) {
        var trabajo = Data(repeating: CERO, count: tamVecEntradaBytes)
        var p = 0
        var n = 0
        while p < tamVecEntradaBytes {
            let periodo = buf.subdata(in:p..<p+tamPeriodo)
            if periodo == ceroData {
                p += tamPeriodo
                continue
            }
            var i = 0
            var q = 0
            while i < n && periodo.lexicographicallyPrecedes(trabajo.subdata(in:q..<q+tamPeriodo)) {
                i += 1
                q += tamPeriodo
            }

            if i < n && periodo == trabajo.subdata(in:q..<q+tamPeriodo) {
                p += tamPeriodo
                continue
            }

            if i == n {
                q = n * tamPeriodo
                trabajo.replaceSubrange(q..<q+tamPeriodo, with: periodo)
            } else {
                var j = tamVector-1
                while j > i {
                    q = j * tamPeriodo
                    trabajo.replaceSubrange(q..<q+tamPeriodo, with: trabajo.subdata(in:q-tamPeriodo..<q))
                    j -= 1
                }
                q = i * tamPeriodo
                trabajo.replaceSubrange(q..<q+tamPeriodo, with: periodo)
            }
            n += 1
            p += tamPeriodo
        }
        return (n, trabajo.subdata(in:0..<tamVecSalidaBytes))
    }

Esta versión era extremadamente lenta, por sobre los 2 minutos para el archivo de 1 millón de registros, así que decidí trabajar con arreglos de bytes de este modo:

    func ordenarVector(_ buf: [Int8],  _ trabajo : inout [Int8]) -> Int {
        var p = posVector
        var n = 0
        let tope = largoLinea-1
        while p < tope {
            let periodo = buf[p..<p+tamPeriodo]
            if periodo == ceroData {
                p += tamPeriodo
                continue
            }
            var i = 0
            var q = 0
            while i < n && periodo.lexicographicallyPrecedes(trabajo[q..<q+tamPeriodo]) {
                i += 1
                q += tamPeriodo
            }

            if i < n && periodo == trabajo[q..<q+tamPeriodo] {
                p += tamPeriodo
                continue
            }

            if i == n {
                q = n * tamPeriodo
                trabajo[q..<q+tamPeriodo] = periodo
            } else {
                var j = tamVector-1
                while j > i {
                    q = j * tamPeriodo
                    trabajo[q..<q+tamPeriodo] = trabajo[q-tamPeriodo..<q]
                    j -= 1
                }
                q = i * tamPeriodo
                trabajo[q..<q+tamPeriodo] = periodo
            }
            n += 1
            p += tamPeriodo
        }
        return n
    }

Finalmente al hacer profiling detecté que los slices, por ejemplo trabajo[q..<q+tamPeriodo] generan varias estructuras internos y muchos mallocs y frees.

La solución final, quedó así:

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

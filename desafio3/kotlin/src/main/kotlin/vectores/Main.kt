package vectores

import java.io.File
import java.util.*

val tamVector   = 23
val posVector   = 9
val tamPeriodo  = 6
val nInsts      = 6
val largoLinea  = posVector + nInsts * tamVector * tamPeriodo
val relleno = " ".repeat(tamVector* tamPeriodo)
val ceros = "000000"

@Suppress("UNUSED_EXPRESSION")
fun showTime(block: () -> Unit) {
    val t0 = System.nanoTime()
    block()
    val t1 = System.nanoTime()
    val elapsed = t1 - t0
    val msecs = elapsed / 1000000
    val mins = (msecs % 3600000) / 60000
    val secs = ((msecs % 3600000) % 60000) / 1000.0

    println("tiempo ocupado :" + "%02d:%02.3f".format(mins, secs))
}

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

fun procesarLinea(nl:Int, linea:String) : String {
    if (linea.length == largoLinea)
        return ordenarVector(linea)
    else {
        println("error en linea: "+nl)
        return linea
    }
}

fun procesarVectores(archivoEntrada:String, archivoSalida:String) {
    val salida = File(archivoSalida).printWriter()
    var nl = 0
    File(archivoEntrada).forEachLine { linea ->
        salida.println(procesarLinea(nl, linea))
        nl += 1
    }
    salida.close()
}

fun main(args: Array<String>)  {
  if (args.isEmpty() || args.size != 2) {
  	println("Uso: ordenar_vector archivo_entrada archivo_salida")
  } else {
  	showTime { procesarVectores(args[0], args[1]) }
  }

}
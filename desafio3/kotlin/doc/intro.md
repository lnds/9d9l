# Ordenar Vectores en Kotlin

Esta versión en Kotlin es sorprendentemente rápida.

## Archivos

El manejo de archivos en Kotlin es sencillo y han extendido el uso de las bibliotecas estándares de Java con métodos adicionales, lo que hace muy simple iterar sobre lineas de archivos.

La lectura de archivos es muy simple:

	fun procesarVectores(archivoEntrada:String, archivoSalida:String) {
	    val salida = File(archivoSalida).printWriter()
	    var nl = 0
	    File(archivoEntrada).forEachLine { linea ->
	        salida.println(procesarLinea(nl, linea))
	        nl += 1
	    }
	    salida.close()
	}


## Slices y Strings

En la solución Kotlin tratamos cada linea como strings, no como arreglos de bytes como en C. 

Ordenar los periodos se hace de este modo:

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

Que resulta ser bastante compacto.

# Propuesto

¿Se puede optimizar más este código?
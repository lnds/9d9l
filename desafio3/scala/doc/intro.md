# ordenar_vector en Scala

## Archivos

Usamos varias características de Scala para abrir los archivos, usamos Try para verificar el éxito de la operación.
Usamos un objeto tipo Source para leer las lineas, las que indexamos gracias al método zipWithIndex de los iteradores de Scala:

    Try(Source.fromFile(new File(entrada), encoding, bufferSize)) match {
      case Failure(e) =>  println(f"No pudo abrir archivo $entrada%s, causa: $e%s")
      case Success(rdr) => 
        Try(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(salida)))) match {
          case Failure(e) => println(f"No pudo crear archivo $salida%s, causa: $e%s")
          case Success(wrt) => 
            rdr.getLines.zipWithIndex.foreach { 
              case (linea, nl) =>
                wrt.write( filtrarLinea(linea, nl) )
                wrt.newLine()
            }
            wrt.close()
        }
    }

## Slices y Arreglos de Bytes

Por fortuna Scala tiene un método slice que es bastante eficiente. Esto permite extraer los periodos.
Vamos metiendo cada periodo en un conjunto ordenado, con esto eliminamos duplicados y nos evitamos el paso de ordenar el arreglo después.

    val encabezado = linea.slice(0, largoEncabezado)
    val myOrdering = Ordering.fromLessThan[String](_ > _)
    var periodos = SortedSet.empty[String](myOrdering)
    var pos = posVector
    while (pos < largoLinea) {
      if (!linea.regionMatches(pos, ceros, 0, tamPeriodo)) 
         periodos += linea.slice(pos, pos+tamPeriodo)
      pos += tamPeriodo
    }

Notar que usamos la función String.regionMatches() para comparar cada segmento con ceros ("000000") para optimizar el código.
    
Para armar el string de resultado usamos Stings.Ops que nos permite repetir string con el operador *.

    val len = periodos.size
    encabezado + (
      if (len == 0) 
          "N" + (" " * tamRelleno)
      else if (len > tamVector)
          "S" + " " * tamRelleno
      else 
          "D" +  periodos.mkString + " "*(tamRelleno-len*tamPeriodo)
    )

## Propuesto

¿Se puede implementar de manera más funcional esta solución?


# ordenar_vector en F#

## Archivos

El manejo de archivos en F# se basa en .Net, con la diferencia que en este caso usamos una función para generar una secuencia lazy con las lineas a leer:

	let leer nomarch =
	    seq {
	        use fr = new StreamReader(new FileStream(nomarch, FileMode.Open, FileAccess.Read, FileShare.Read, 4096))
	        while not fr.EndOfStream do 
	            yield fr.ReadLine()
	    }

Para leer creamos un "sink" donde dejamos las lineas procesadas:

	let escribir nomarch (lineas : string seq) =
	    use fw = new StreamWriter(nomarch, false, Encoding.ASCII,  4096)
	    for linea in lineas
        	do fw.WriteLine linea

De este modo procesar el archivo completo se expresa de una manera muy elegante:

	let procesar_vectores entrada salida =
   		leer entrada |> Seq.mapi filtrar_linea |> escribir salida

Notar como usamos Seq.mapi, esto permite leer secuencialmente linea a linea el archivo e indexar cada linea con un número, que es usado para desplegar error en la función filtrar_linea:

	let filtrar_linea n (linea : string)=
	    if String.length linea <> LARGO_LINEA then 
	        printfn "error en linea: %d" n
	        linea
	    else 
	        linea.Substring(0, POS_VECTOR) + ordenar_periodos (linea.Substring(POS_VECTOR))


## Slices y Strings

En la solución F# tratamos cada linea como strings, no como arreglos de bytes como en C. 

Ordenar los periodos se hace de este modo:


	let ordenar_periodos linea = 
	    let periodos = separar_periodos linea |> Set.ofSeq  |> Seq.sortDescending |> Seq.toArray

	    let len = Seq.length periodos
	    if len = 0 then "N".PadRight(TAM_VECTOR+1)
	    else if len > ELEMENTOS_VECTOR then "S".PadRight(TAM_VECTOR+1)
	    else ("D" + (periodos |> String.Concat)).PadRight(TAM_VECTOR+1)

La función separar_periodos es la siguiente

	let separar_periodos (linea:string) =
	    seq {
	        for p in [0..TAM_PERIODO..(TAM_VECTOR_ENTRADA-TAM_PERIODO)] do
	            let periodo = linea.Substring(p, TAM_PERIODO)
	            if periodo <> CERO then
	                yield periodo
	    }

Esta función es el cuello de botella de esta solución. Trate de usar algo parecido a la funcion regionMatches de java, pero no pude lograr más velocidad con eso. 

# Profiling

Para mejorar los tiempos usé mono --profile=log y luego visualicé los reportes con mprof-report output.mlpd.

# Propuesto

¿Se puede optimizar más este código?
# ordenar_vector en F#

## Archivos

El manejo de archivos en F# se basa en .Net, con la diferencia que en este caso usamos una función para generar una secuencia lazy con las lineas a leer:

	let leer nomarch = seq {
	    use fr = new StreamReader(new FileStream(nomarch, FileMode.Open, FileAccess.Read, FileShare.Read, BUF_SIZE))
	    let mutable nl = 0
	    while not fr.EndOfStream do 
	        yield (nl, fr.ReadLine())
	        nl <- nl + 1
	}

Esto genera una tupla, con el numero de linea y la linea (esto es para ahorrar la llamada a Seq.mapi en una versión inicial).

Para leer creamos un "sink" donde dejamos las lineas procesadas:

	let escribir nomarch (lineas : string seq) =
	    use fw = new StreamWriter(nomarch, false, Encoding.ASCII,  BUF_SIZE)
	    for linea in lineas
	        do fw.WriteLine linea

De este modo procesar el archivo completo se expresa de una manera muy elegante:

	let procesar_vectores entrada salida =
   		leer entrada |> Seq.map filtrar_linea |> escribir salida

La función filtrar_linea recibe una tupla con el numero de linea y el string con el contenido de la misma:


	let filtrar_linea (n:int, linea : string)=
	    if String.length linea <> LARGO_LINEA then 
	        printfn "error en linea: %d" n
	        linea
	    else 
	        linea.Substring(0, POS_VECTOR) + ordenar_periodos (linea)


## Slices y Strings

En la solución F# tratamos cada linea como strings, no como arreglos de bytes como en C. 

Ordenar los periodos se hace de este modo:

	let ordenar_periodos (linea:string) = 
	    let periodos = separar_periodos linea |> Seq.distinct |> Seq.toList  

	    let len = Seq.length periodos
	    if len = 0 then "N".PadRight(PAD_SIZE)
	    else if len > ELEMENTOS_VECTOR then "S".PadRight(PAD_SIZE)
	    else ("D" + (periodos |> Seq.sortDescending |> String.Concat)).PadRight(PAD_SIZE)

La función separar_periodos es un generador escrito de la siguiente manera:

	let separar_periodos (linea:string) = seq {
	    let mutable p = POS_VECTOR
	    while p < LARGO_LINEA do
	        if no_es_cero linea p then
	            yield linea.Substring(p, TAM_PERIODO)
	        p <- p + TAM_PERIODO
	}
    

Esta función es el cuello de botella de esta solución. La clave para reducir el tiempo fue la función no_es_cero, que quedó escrita de forma imperativa para alcanzar la maxima velocidad:

	let no_es_cero  (linea:string) (pos:int) = 
	    let mutable result = false
	    let mutable i = pos
	    let top = pos+TAM_PERIODO-1
	    while i < top && result = false do
	        if linea.[i] <> '0' then 
	            result <- true
	        i <- i + 1
	    result


# Profiling

Para mejorar los tiempos usé mono --profile=log y luego visualicé los reportes con mprof-report output.mlpd.

# Propuesto

¿Se puede optimizar más este código?
open System
open System.IO
open System.Text


let POS_VECTOR = 9
let ELEMENTOS_VECTOR = 23
let TAM_PERIODO = 6
let CANT_INSTITUCIONES = 6
let TAM_VECTOR = ELEMENTOS_VECTOR * TAM_PERIODO
let TAM_VECTOR_ENTRADA = TAM_VECTOR * CANT_INSTITUCIONES
let LARGO_LINEA = POS_VECTOR + TAM_VECTOR_ENTRADA
let TAM_SALIDA = POS_VECTOR + 1 + TAM_VECTOR

let BUF_SIZE = 65536

let leer nomarch = seq {
    use fr = new StreamReader(new FileStream(nomarch, FileMode.Open, FileAccess.Read, FileShare.Read, BUF_SIZE))
    let mutable nl = 0
    while not fr.EndOfStream do 
        yield (nl, fr.ReadLine())
        nl <- nl + 1
}

let escribir nomarch (lineas : string seq) =
    use fw = new StreamWriter(nomarch, false, Encoding.ASCII,  BUF_SIZE)
    for linea in lineas
        do fw.WriteLine linea

let no_es_cero  (linea:string) (pos:int) = 
    let mutable result = false
    let mutable i = pos
    let top = pos+TAM_PERIODO-1
    while i < top && result = false do
        if linea.[i] <> '0' then 
            result <- true
        i <- i + 1
    result

let separar_periodos (linea:string) = seq {
    let mutable p = POS_VECTOR
    while p < LARGO_LINEA do
        if no_es_cero linea p then
            yield linea.Substring(p, TAM_PERIODO)
        p <- p + TAM_PERIODO
}
    
let PAD_SIZE = TAM_VECTOR+1

let ordenar_periodos (linea:string) = 
    let periodos = separar_periodos linea |> Seq.distinct |> Seq.toList  

    let len = Seq.length periodos
    if len = 0 then "N".PadRight(PAD_SIZE)
    else if len > ELEMENTOS_VECTOR then "S".PadRight(PAD_SIZE)
    else ("D" + (periodos |> Seq.sortDescending |> String.Concat)).PadRight(PAD_SIZE)


let filtrar_linea (n:int, linea : string)=
    if String.length linea <> LARGO_LINEA then 
        printfn "error en linea: %d" n
        linea
    else 
        linea.Substring(0, POS_VECTOR) + ordenar_periodos (linea)


let procesar_vectores entrada salida =
    try
        leer entrada |> Seq.map filtrar_linea |> escribir salida
    with
        | :? System.IO.FileNotFoundException -> 
            printfn "no pudo abrir archivo %s"  entrada

[<EntryPoint>]
let main(argv: string[]) = 
    if argv.Length <> 2 then
        printfn "uso: ordenar-vector archivo_entrada archivo_salida"
    else
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        procesar_vectores argv.[0] argv.[1]
        stopWatch.Stop()
        printfn "tiempo ocupado: %d:%d:%d.%d" stopWatch.Elapsed.Hours stopWatch.Elapsed.Minutes stopWatch.Elapsed.Seconds  stopWatch.Elapsed.Milliseconds

    0
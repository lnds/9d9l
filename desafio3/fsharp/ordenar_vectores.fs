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

let CERO = "000000"

let leer nomarch =
    seq {
        use fr = new StreamReader(new FileStream(nomarch, FileMode.Open, FileAccess.Read, FileShare.Read, 4096))
        while not fr.EndOfStream do 
            yield fr.ReadLine()
    }

let escribir nomarch (lineas : string seq) =
    use fw = new StreamWriter(nomarch, false, Encoding.ASCII,  4096)
    for linea in lineas
        do fw.WriteLine linea

let separar_periodos (linea:string) =
    seq {
        for p in [0..TAM_PERIODO..(TAM_VECTOR_ENTRADA-TAM_PERIODO)] do
            let periodo = linea.Substring(p, TAM_PERIODO)
            if periodo <> CERO then
                yield periodo
    }
    
let ordenar_periodos linea = 
    let periodos = separar_periodos linea |> Seq.toList |> Seq.distinct  |> Seq.sortDescending |> Seq.toArray

    let len = Seq.length periodos
    if len = 0 then "N".PadRight(TAM_VECTOR+1)
    else if len > ELEMENTOS_VECTOR then "S".PadRight(TAM_VECTOR+1)
    else ("D" + (periodos |> String.Concat)).PadRight(TAM_VECTOR+1)


let filtrar_linea n (linea : string)=
    if String.length linea <> LARGO_LINEA then 
        printfn "error en linea: %d" n
        linea
    else 
        linea.Substring(0, POS_VECTOR) + ordenar_periodos (linea.Substring(POS_VECTOR))


let procesar_vectores entrada salida =
   leer entrada |> Seq.mapi filtrar_linea |> escribir salida

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
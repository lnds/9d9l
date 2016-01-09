open System

let shuffle = List.sortBy(fun _ -> System.Guid.NewGuid())

let validar tam accion =
    let cars = [for c in accion -> c]
    if List.exists (fun c -> not (Char.IsDigit c)) cars then List.empty<int>
    else 
        let org = List.map (fun c -> (int c) - (int '0')) cars
        let num = List.distinct org
        if (List.length org <> List.length num) || (List.length num <> tam) then List.empty<int>
        else num

let comparar num sec = 
    let rec tyf ns xs ys =
        match (ns, xs, ys) with
        | ([], _, _) -> (0,0)
        | (n::ns, x::xs, ys) -> 
            let (t,f) = tyf ns xs ys
            if n = x then (t,f+1) else (if List.exists (fun x -> x = n) ys then (t+1,f) else (t,f))
    tyf num sec sec 

let rec jugar tam sec i =
    printfn "Ingresa una secuencia de %A dígitos distintos (o escribe salir):" tam
    let accion = Console.ReadLine()
    if accion = "salir" then ()
    else 
        let num = validar tam accion
        if List.isEmpty num then 
            printfn "error!\n"
            jugar tam sec (i+1)
        else 
            printfn "ingresaste: %A" num
            let (toques, famas) = comparar num sec
            printfn "resultado: %A Toques, %A Famas\n" toques famas
            if tam = famas then
                printfn "Ganaste! Acertaste al intento %A! La secuencia era %A." i sec
            else
                jugar tam sec (i+1)

let mostrarReglas tam =
    printfn "Bienvenido a Toque y Fama."
    printfn "=========================="
    printfn "En este juego debes tratar de adivinar una secuencia de %d dígitos generada por el programa." tam
    printfn "Para esto ingresas %d dígitos distintos con el fin de adivinar la secuencia."  tam
    printfn "Si has adivinado correctamente la posición de un dígito se produce una Fama."
    printfn "Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n" 
    printfn "Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:"
    printfn "tu ingresaste [4, 0, 8, 6, 3]" 
    printfn "resultado: 2 Toques 2 Famas\n"


[<EntryPoint>]
let main (argv :string[]) = 
    let tam = 5
    let sec = [ 0 .. 9 ]  |> shuffle |> Seq.take tam |> List.ofSeq
    mostrarReglas tam 
    jugar tam sec 0
    0
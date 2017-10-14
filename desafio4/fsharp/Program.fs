namespace Huffman

module Program = 

    open Huffman.Trees

    let usage () = printfn "uso: huffman [-c|-d] entrada salida"
    
    
    [<EntryPoint>]
    let main argv = 
        if argv.Length <> 3 then
           usage()
        else
           match List.ofArray argv with
             | param :: input :: output :: [] when param = "-c" -> compress input output 
             | param :: input :: output :: [] when param = "-d" -> decompress input output
             | _ -> usage()
        0 // return an integer exit code

namespace Huffman

module Trees =
    
    open System.IO
    open Huffman.IO

    type Tree =
        | Leaf of byte * frequency:int
        | Node of left:Tree  * right: Tree  * frequency:int
    
        member this.Freq() =
            match this with
            | Leaf(_, f) -> f
            | Node(_,_,f) -> f
            
        member this.WriteTo(writer:BitOutputStream) =
            match this with
            | Leaf(s, _) ->
                writer.WriteBit(true)
                writer.WriteByte(s)
            | Node(l, r, _) ->
                writer.WriteBit(false)
                l.WriteTo(writer)
                r.WriteTo(writer)
                
        member this.ReadChar(reader:BitInputStream) =
            match this with
            | Leaf(s, _) -> s
            | Node(left, right, _) ->
                if reader.ReadBit() then
                    right.ReadChar(reader)
                else
                    left.ReadChar(reader)
               
        member this.Parse(reader:BitInputStream) (writer:Stream) =
            while not reader.Eof do
                let ch = this.ReadChar(reader)
                writer.WriteByte(ch)
        
    let calcFreqs (l:byte list) = 
        l |> Seq.groupBy(fun c -> c) |> Seq.map (fun (c, l) -> c, (List.ofSeq l).Length) |> Seq.toList
        
    let buildTree (freqs: (byte*int) list) =
        let sort (tree: Tree list) =
            tree |> List.sortBy(fun i -> i.Freq())
        let rec loop (tree: Tree list) =
            match sort tree with
            | left::right::[] -> Node(left, right, left.Freq() + right.Freq())
            | left::right::tail -> 
                let node = Node(left,  right, left.Freq() + right.Freq())
                loop (node :: tail)
            | [node] -> node
            | [] -> failwith "empty tree list!"
        freqs |> Seq.map Leaf |> List.ofSeq |> loop 
        
    let makeCodes (tree:Tree) =
        let rec loop tree code =
            match tree with
            | Leaf(s, _) -> 
                [(s, code)]
            | Node(l, r, _) ->
                let a = loop l (code @ [false])
                let b = loop r (code @ [true])
                List.append a b
        loop tree [] |> dict
        
    
    let rec readTree (reader:BitInputStream) =
        let b = reader.ReadBit()
        if b then 
            readLeaf(reader)
        else
            readNode(reader)
   
    and readLeaf(reader:BitInputStream) =
           let sym = reader.ReadByte()
           Leaf(sym, -1)
                       
    and readNode(reader:BitInputStream) = 
                let left = readTree(reader)
                let right = readTree(reader)
                Node(left, right, -1)
        
    let compress (input:string) (output:string) = 
        let bs = File.ReadAllBytes input
        let tree =  List.ofArray bs |> calcFreqs |> buildTree
        let codes = makeCodes tree
        use writer = new BitOutputStream(output)
        tree.WriteTo(writer)
        for b in bs do
            writer.WriteBits(codes.[b])
            
    let decompress (input:string) (output:string) =
        use reader = new BitInputStream(input)
        let tree = readTree(reader)
        let writer = File.OpenWrite(output)
        tree.Parse reader writer
        
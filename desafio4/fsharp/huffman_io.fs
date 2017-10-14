namespace Huffman

module IO = 


    open System
    open System.IO
    
    type bit = bool
    
    type BitInputStream(file: string) = 
        
        let stream = File.OpenRead(file)
        let buffer = ref 0uy
        let len = ref 0
        let pos = ref 0L
        
        let loadBuffer() =
            if stream.Position >= stream.Length then buffer := 0uy
            len := 8
            let b = stream.ReadByte()
            if b = -1 then
                buffer := 0uy
            buffer := byte b

    
        let readBit() =
            len := !len - 1
            let mask = 1 <<< !len
            let v = !buffer &&& (byte mask)
            pos := !pos + 1L
            v >= 1uy
            
        member this.Eof with
            get() = 
                stream.Position >= stream.Length && !len <= 0
            
        member this.ReadBit() =
            if this.Eof then
                failwith("eof")
            if !len <= 0 then loadBuffer()
            readBit()
                
        member this.ReadByte() = 
            if this.Eof then 
               failwith("eof!")
            if !len <= 0 then loadBuffer()   
            let x = !buffer
            if !len = 8 then
                loadBuffer()
                x
            else 
                let v = x <<< (8 - !len)
                let temp = !len
                loadBuffer()
                len := temp
                v ||| (!buffer >>> !len)
                
        member this.Close() =
            stream.Close()
                    
    
        interface IDisposable with
        
            member this.Dispose() =
                this.Close()
        
    
    
    type BitOutputStream(file: string) = 
    
        let stream = File.OpenWrite(file)
        let buffer = ref 0uy
        let len = ref 0
        
        let flush() = 
            while !len < 8 do
                buffer := !buffer <<< 1
                buffer := !buffer ||| 0uy
                len := !len + 1
            stream.WriteByte(!buffer)
            stream.Flush()
            buffer := 0uy
            len := 0
            
        let mustFlush() = !len >= 8
        
        member this.Flush() = 
            if mustFlush() then flush()
        
        member this.WriteBit(b:bit) =
            let v = if b then 1uy else 0uy
            buffer := ((!buffer) <<< 1) ||| v
            len := !len + 1
            this.Flush()
            
        member this.WriteBits(bits:bit list) =
            for b in bits do
                this.WriteBit b
    
        member this.WriteByte(b:byte) = 
           for i in [0..7] do
                let v = ((b >>> (8 - i - 1)) &&& 1uy) = 1uy
                this.WriteBit(v)
        
        member this.Close() =
            if !len > 0 then flush()
            stream.Close()
        
        interface IDisposable with
            
                member this.Dispose() =
                    this.Close()
                
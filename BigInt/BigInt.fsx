﻿#load "Library1.fs"
open BigInt

type BigInt (n:int) =
    static member valueAsString n = Array.ofSeq n |> Array.rev |> Seq.fold (fun acc x -> acc + x.ToString()) ""
    member this.Value = n.ToString().ToCharArray() 
                                |> Seq.map (fun x -> int (x.ToString()));

    member this.getSameSizeValues (y:BigInt)= 
        let lengthDiff = (Seq.length this.Value) - (Seq.length y.Value)
        if lengthDiff > 0 then
            (this.Value, Seq.append (Array.zeroCreate lengthDiff) y.Value)
        elif lengthDiff < 0 then
            ((Seq.append (Array.zeroCreate (lengthDiff * (-1))) this.Value), y.Value)
        else
            (this.Value, y.Value)
    member this.add (y:BigInt) = 
            let (a,b) = this.getSameSizeValues (y:BigInt)
            let (rest, anwser) = (Seq.fold (fun ((rest:int), (elements:int List)) ((first:int), (second:int)) -> 
                                    let sum = first + second + rest
                                    let newRest = if sum - 10 < 0 then 0 else (sum - 10)
                                    (newRest, (sum :: elements))) 
                                 (0, [])
                                 (Seq.zip a b))
            BigInt.valueAsString anwser
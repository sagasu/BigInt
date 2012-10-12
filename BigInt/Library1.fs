namespace BigInt

type BigInt (n:int) =
    static member valueAsString n = Seq.fold (fun acc x -> acc + x.ToString()) "" n
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
        this.performOperation y (fun a b c -> a + b + c) 0
    member this.multiply (y:BigInt) = 
        this.performOperation y (fun a b c -> a * b * c) 1

    member private this.performOperation (y:BigInt) opeartion neutralElement =
            let (a,b) = this.getSameSizeValues (y:BigInt)
            let (rest, anwser) = (Seq.fold (fun ((rest:int), (elements:int List)) ((first:int), (second:int)) -> 
                                    let sum = (opeartion first second rest) 
                                    let newRest = if sum - 10 < 0 then 0 else (sum / 10)
                                    (newRest, (sum % 10 :: elements))) 
                                 (neutralElement, [])
                                 (Array.ofSeq (Seq.zip a b) |> Array.rev))
            BigInt.valueAsString (if rest = 0 then anwser else (rest :: anwser))

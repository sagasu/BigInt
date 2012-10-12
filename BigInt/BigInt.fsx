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
        let reversedY = Array.ofSeq y.Value |> Array.rev
        let anwser = (Seq.fold (fun (elements:int List) (xElement:int) -> 
                                                let (comRest, comResult) = (Seq.fold (fun ((innerRest:int), (innerElements:int List)) yElement -> 
                                                                                            let product = xElement * yElement + innerRest
                                                                                            let newRest = if product - 10 < 0 then 0 else (product / 10)
                                                                                            (newRest, (product % 10 :: innerElements)))
                                                                                     (0,[]) 
                                                                                     reversedY)
                                                (if comRest = 0 then comResult else (comRest :: comResult)))
                                        []
                                        this.Value)
        BigInt.valueAsString anwser

    member private this.performOperation (y:BigInt) opeartion neutralElement =
            let (a,b) = this.getSameSizeValues (y:BigInt)
            let (rest, anwser) = (Seq.fold (fun ((rest:int), (elements:int List)) ((first:int), (second:int)) -> 
                                    System.Console.WriteLine("{0} {1} {2}", rest, first, second);
                                    let operationResult = (opeartion first second rest) 
                                    let newRest = if operationResult - 10 < 0 then 0 else (operationResult / 10) // operationResult is an integer so a result is always an integer floor
                                    (newRest, (operationResult % 10 :: elements))) 
                                 (neutralElement, [])
                                 (Array.ofSeq (Seq.zip a b) |> Array.rev))
            BigInt.valueAsString (if rest = 0 then anwser else (rest :: anwser))

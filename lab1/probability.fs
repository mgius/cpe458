let r = System.Random()

let distribution1 () = 
    let randNum = r.NextDouble()
    match randNum with
        | randNum when randNum < 0.1 -> 0
        | randNum when randNum < 0.6 -> 1
        | randNum when randNum < 0.65 -> 2
        | _ -> 3

let distribution2 () = 
    let randNum = r.NextDouble()
    match randNum with
        | randNum when randNum < 0.2 -> 0
        | randNum when randNum < 0.9 -> 1
        | randNum when randNum < 0.95 -> 2
        | _ -> 3


let rec manySamples i dist =
    match i with
        | 1 -> dist ()
        | _ -> dist () + manySamples (i - 1) dist

printfn "%d" (manySamples 50 distribution1)
printfn "%d" (manySamples 50 distribution2)

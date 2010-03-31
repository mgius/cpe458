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

let distInner randNum range: float array =
    if range.Length >= 1 && randNum < range.[0] then
        0
    else
        (distInner range.[1..] randNum) + 1

let distribution range = 
    distInner (r.NextDouble()) range 

let rec manySamplesOld i dist =
    match i with
        | 1 -> dist ()
        | _ -> dist () + manySamples (i - 1) dist

let rec manySamples i dist range =
    match i with
        | 1 -> dist ()
        | _ -> dist () + manySamples (i - 1) dist

//printfn "%d" (manySamples 50 distribution1)
//printfn "%d" (manySamples 50 distribution2)
printfn "%d" (manySamples 50 distribution [| 0.3, 0.9 |])

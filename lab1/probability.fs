// This appears to be some sort of
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

let rec distInner randNum (range : float array) =
    if randNum < range.[0] then
        0
    elif range.Length = 1 then
        1
    else
        (distInner randNum range.[1..]) + 1

let distribution range = 
    distInner (r.NextDouble()) range 

let rec manySamplesOld i dist =
    match i with
        | 1 -> dist ()
        | _ -> dist () + manySamplesOld (i - 1) dist

let rec manySamples i range dist =
    match i with
        | 1 -> dist range
        | _ -> dist range + manySamples (i - 1) range dist

//printfn "%d" (manySamples 50 distribution1)
//printfn "%d" (manySamples 50 distribution2)
//printfn "%d" (manySamples 50 [| 0.3; 0.9 |] distribution )

let result = List.sort [for i in 1..10000 -> (manySamples 100 [| 0.3; 0.9 |] distribution )]

let rec handleList (li : int list) currentCount currentNum = 
    if li.Tail = [] then // last one, print out stuff
        printfn "%04d, %d" currentNum currentCount
    elif li.Head = li.Tail.Head then // These two elements are the same, so collapse them
        handleList li.Tail (currentCount + 1) currentNum
    else 
        printfn "%04d, %d" currentNum currentCount
        handleList li.Tail 1 li.Tail.Head

handleList result 1 result.Head

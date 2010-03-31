let r = System.Random()

let distribution1 = 
    let randNum = r.NextDouble()
    match randNum with
        | randNum when randNum < 0.1 -> 0
        | randNum when randNum < 0.6 -> 1
        | randNum when randNum < 0.65 -> 2
        | _ -> 3

let rec manySamples i =
    match i with
        | 0 -> 0
        | _ -> (distribution1) + manySamples (i - 1)

printfn "%d" (manySamples 5)

for i = 0 to 10 do
    printfn "%f" (r.NextDouble())

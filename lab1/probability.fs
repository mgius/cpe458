let randomReturn = 
    let x = System.Random().NextDouble()
    match x with
        | x when x < 0.1 -> 0
        | x when x < 0.6 -> 1
        | x when x < 0.65 -> 2
        | _ -> 3

let randomReturn2 = 
    let x = System.Random().NextDouble()
    if x < 0.1 then 0
    elif x < 0.6 then 1
    elif x < 0.65 then 2
    else 3



for i = 1 to 10 do
    printfn "%d" randomReturn2

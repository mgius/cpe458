let randomReturn = 
    let x = System.Random().NextDouble()
    match x with
        | x when x < 0.1 -> 0
        | x when x < 0.6 -> 1
        | x when x < 0.65 -> 2
        | _ -> 3

for i = 1 to 10 do
    printfn "%d" randomReturn

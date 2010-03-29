let rec growth years balance = 
    match years with 
        | 0 -> balance
        | _ -> 1.05 * growth (years - 1) balance

printfn "%f" (growth 50 100.0)

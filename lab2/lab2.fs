let r = System.Random()
let rTest = System.Random(1443) // This seed gives diff results for first two

let f1 x = 
    3.0 * (x ** 2.0) + 14.0

let differ func x = 
    func (x + 1.0e-9) - func x

let oneStepInner (rGen : System.Random) input = 
    match rGen.Next(2) with
        | 0 -> input * 1.01
        | 1 -> input * 0.99
        | _ -> input // can't happen

let oneStep = oneStepInner r
let oneStepTester = oneStepInner rTest


f1 5.0 |> printfn "89.0 is %f"
differ f1 1.0 |> printfn ".000000003 is %f"
oneStepTester 1000.0 |> printfn "1010.0 is %f"
oneStepTester 1000.0 |> printfn "990.0 is %f"

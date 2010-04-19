open Xunit
open FsxUnit.Syntax
open System

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


(differ f1 1.0) |> printfn "%.9f"
[<Fact>]
let f1Test () =
    should equal (f1 5.0) 89.0

[<Fact>]
let differTest () =
    should equal (differ f1 1.0) 0.000000003

//[<Fact>]
//let oneStepTests () =
//    should equal (oneStepTester 1000.0) 1010.0
//    should equal (oneStepTester 1000.0) 990.0

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

let nStepsInner innerOneStep d i  =
    List.fold (fun acc x -> (innerOneStep acc)) d [for j in 1..i -> j]

let nSteps = nStepsInner oneStep
let nStepsTester = nStepsInner oneStepTester

(nStepsTester 1.0 5) |> printfn "%f"

let seqn20 = seq { for i in 1..1000 -> (nSteps 1.0 20) }
let seqn20counted = Seq.countBy (fun elem -> elem) seqn20

let seqn40 = seq { for i in 1..1000 -> (nSteps 1.0 40) }
let seqn40counted = Seq.sort (Seq.countBy (fun elem -> elem) seqn40)

let printSeq seq1 = Seq.iter (printf "%A \n") seq1; printfn ""

printSeq seqn20
printfn "\n\n"
printSeq seqn40



//(differ f1 1.0) |> printfn "%.9f"
//[<Fact>]
//let f1Test () =
//    should equal (f1 5.0) 89.0
//
//[<Fact>]
//let differTest () =
//    should equal (differ f1 1.0) 0.000000006
//
//[<Fact>]
//let oneStepTests () =
//    should equal (oneStepTester 1000.0) 1010.0
//    should equal (oneStepTester 1000.0) 990.0

#light
open Xunit
open FsxUnit.Syntax
open System

open deltahedging

(* for each function, verify the signature then test it *)
[<Fact>]
let eAllHeadsTest () =
   ignore (eAllHeads : event)
   eAllHeads () |> should equal true

[<Fact>]
let eAllTailsTest () =
   ignore (eAllTails : event)
   eAllTails () |> should equal false

[<Fact>]
let eAlternatingTest () =
   ignore (eAlternating : event)
   eAlternating 0 |> should equal true
   eAlternating 1 |> should equal false
   (eAlternating 1) |> should not_equal (eAlternating 2)

[<Fact>]
let makeERandomTest () =
   ignore (makeERandom : unit -> event)
   let eRandom1 = makeERandom ()
   let eRandom2 = makeERandom ()
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom2 1000 |> should equal (eRandom2 1000)

   (* Run the first random variable 1000 times, and verify
      that the number of trues is roughly 500 *)
   let results = seq { for i in 1..1000 -> (eRandom1 i) }
   let aggregate = Seq.countBy (fun elem -> elem) results
   let findFunc (elem : bool * int) = fst elem
   let trues = snd (Seq.find (fun elem -> fst elem) aggregate)
   trues > 450 |> should equal true
   trues < 550 |> should equal true

[<Fact>]
let makeERandPTest () =
   ignore (makeERandP : double -> event)
   let eRandom1 = makeERandP 0.4
   let eRandom2 = makeERandP 0.9
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom1 2 |> should equal (eRandom1 2)

   (* Run the first random variable 1000 times, and verify
      that the number of trues is roughly 400 *)
   let results = seq { for i in 1..1000 -> (eRandom1 i) }
   let aggregate = Seq.countBy (fun elem -> elem) results
   let findFunc (elem : bool * int) = fst elem
   let trues = snd (Seq.find (fun elem -> fst elem) aggregate)
   trues > 350 |> should equal true
   trues < 450 |> should equal true

   

[<Fact>]
let makeERandTTest () =
   (* Run this 1000 times, verify that at least 650 of the 
      runs are matching the previous run *)
   ignore (makeERandT : unit -> event)
   let randT = makeERandT ()
   let lastResult = ref((randT 0))
   let results = seq { for i in 1..1000 -> (randT i) }
   
   let aggregate = Seq.countBy (fun elem ->
                                  let temp = (!lastResult = elem)
                                  lastResult := elem
                                  temp)
                               results
   let findFunc (elem : bool * int) = fst elem
   snd (Seq.find (fun elem -> fst elem) aggregate) > 650 |> should equal true
   
[<Fact>]
let forceEPartsTest () =
   ignore (forceEParts : int -> bool array -> event -> event)
   (* Create a random variable, call it a number of times, then
      overlay an array and check to see if it took hold *)
   let eRandom1 = makeERandom ()
   let array1 = [|true; false; true; false; true;|]
   for i = 1 to 10 do (eRandom1 i)
   let eRandom2 = forceEParts 2 array1 eRandom1
   for i = 2 to 6 do (eRandom2 i) |> should equal array1.[i-2]

   (* Create a random variable, grab a result, invert it, and overlay it *)
   let eRandom3 = makeERandom ()
   let result = eRandom3 1
   let eRandom4 = forceEParts 1 [|not result|] eRandom3
   result |> should not_equal (eRandom4 1)

   (* Original has not changed *)
   result |> should equal (eRandom3 1)
   

//ignore (doubleToRV : double -> rv)
//ignore (rvNCountHeads : int -> rv)
//ignore (rvNCountTails : int -> rv)
//ignore (rvNStock : double -> double -> double -> rvseq)
//ignore (rvPathD : rvseq)
//ignore (unaryLiftRV : (double -> double) -> rv -> rv)
//ignore (binaryLiftRV : (double -> double -> double) -> rv -> rv -> rv)
//ignore (putOptionPayoff : double -> option)
//ignore (callOptionPayoff : double -> option)
//ignore (tabulateN : (int -> 'a) -> int -> 'a list)
//ignore (mean : double list -> double)
//ignore (sampleVar : double list -> double)
//ignore (sampleHeads : int -> double)
//ignore (sampleHeadsMeanAndVariance : int -> int -> (double * double))
//ignore (experiment1 : (double * double) list)

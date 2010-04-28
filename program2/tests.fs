#light
open Xunit
open FsxUnit.Syntax
open System

open deltahedging

(* for each function, verify the signature then test it *)
ignore (eAllHeads : event)
[<Fact>]
let eAllHeadsTest () =
   eAllHeads () |> should equal true

ignore (eAllTails : event)
[<Fact>]
let eAllTailsTest () =
   eAllTails () |> should equal false

ignore (eAlternating : event)
[<Fact>]
let eAlternatingTest () =
   eAlternating 0 |> should equal true
   eAlternating 1 |> should equal false
   (eAlternating 1) |> should not_equal (eAlternating 2)

ignore (makeERandom : unit -> event)
[<Fact>]
let makeERandomTest () =
   let eRandom1 = makeERandom ()
   let eRandom2 = makeERandom ()
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom2 1000 |> should equal (eRandom2 1000)

ignore (makeERandP : double -> event)
[<Fact>]
let makeERandPTest () =
   let eRandom1 = makeERandP 0.4
   let eRandom2 = makeERandP 0.9
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom2 1000 |> should equal (eRandom2 1000)

//ignore (makeERandT : unit -> event)
//ignore (forceEParts : int -> bool array -> event -> event)
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

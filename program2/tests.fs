(* Program 2: Testing Suite
   Mark Gius
   Collaboration with Jesse Tyler
 *)
#light
open Xunit
open FsxUnit.Syntax
open System

open deltahedging

(* for each function, verify the signature then test it *)
[<Fact>]
let test_eAllHeads () =
   ignore (eAllHeads : event)
   eAllHeads () |> should equal true

[<Fact>]
let test_eAllTails () =
   ignore (eAllTails : event)
   eAllTails () |> should equal false

[<Fact>]
let test_eAlternating () =
   (* The real requirement of alternating is that it alternates.  So
      I shouldn't test if specific points are specific states *)
   ignore (eAlternating : event)
   for i=0 to 100 do
   (eAlternating i |> should not_equal (eAlternating (i+1)))

[<Fact>]
let test_makeERandom () =
   ignore (makeERandom : unit -> event)
   let eRandom1 = makeERandom ()
   let eRandom2 = makeERandom ()
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom2 1000 |> should equal (eRandom2 1000)

   (* Run the first random variable 1000 times, and verify
      that the number of trues is roughly 500 
      TODO: make it within 1 std dev*)
   let results = seq { for i in 1..1000 -> (eRandom1 i) }
   let aggregate = Seq.countBy (fun elem -> elem) results
   let findFunc (elem : bool * int) = fst elem
   let trues = snd (Seq.find (fun elem -> fst elem) aggregate)
   trues > 450 |> should equal true
   trues < 550 |> should equal true

[<Fact>]
let test_makeERandP () =
   ignore (makeERandP : double -> event)
   let eRandom1 = makeERandP 0.4
   let eRandom2 = makeERandP 0.9
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 1 |> should equal (eRandom1 1)
   eRandom1 2 |> should equal (eRandom1 2)
   eRandom1 2 |> should equal (eRandom1 2)

   (* Run the first random variable 1000 times, and verify
      that the number of trues is roughly 400 
      TODO: make it within 1 std dev *)
   let results = seq { for i in 1..1000 -> (eRandom1 i) }
   let aggregate = Seq.countBy (fun elem -> elem) results
   let findFunc (elem : bool * int) = fst elem
   let trues = snd (Seq.find (fun elem -> fst elem) aggregate)
   trues > 350 |> should equal true
   trues < 450 |> should equal true

   

[<Fact>]
let test_makeERandT () =
   (* Run this 1000 times, verify that at least 650 of the 
      runs are matching the previous run 
      TODO: make it one std dev *)
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
let test_forceEParts () =
   ignore (forceEParts : int -> bool array -> event -> event)
   (* Create a random variable, call it a number of times, then
      overlay an array and check to see if it took hold *)
   let eRandom1 = makeERandom ()
   let array1 = [|true; false; true; true; false;|]
   for i = 1 to 10 do ignore (eRandom1 i)
   let eRandom2 = forceEParts 2 array1 eRandom1
   for i = 2 to 6 do (eRandom2 i) |> should equal array1.[i-2]
   (* Test for bug of mine.  Wasn't handling timestep <> the overlay start *)
   (eRandom2 1) |> should equal (eRandom2 1)
   (eRandom2 7) |> should equal (eRandom2 7)

   (* Create a random variable, grab a result, invert it, and overlay it *)
   let eRandom3 = makeERandom ()
   let result = eRandom3 1
   let eRandom4 = forceEParts 1 [|not result|] eRandom3
   result |> should not_equal (eRandom4 1)

   (* Original has not changed *)
   result |> should equal (eRandom3 1)

[<Fact>]
let test_doubleToRV () = 
   ignore (doubleToRV : double -> rv)
   let randomV1 = doubleToRV 1.23
   randomV1 (eAllTails 1) |> should equal 1.23
   randomV1 (eAllHeads 1) |> should equal 1.23

[<Fact>]
let test_rvNCountHeads () =
   ignore (rvNCountHeads : int -> rv)
   let headsCount1 = rvNCountHeads 100
   headsCount1 eAllHeads |> should equal 100.0
   headsCount1 eAllTails |> should equal 0.0
   headsCount1 (forceEParts 2 [|false|] eAllHeads) |> should equal 99.0

[<Fact>]
let test_rvNCountTails () =
   ignore (rvNCountTails : int -> rv)
   let headsCount1 = rvNCountTails 100
   headsCount1 eAllTails |> should equal 100.0
   headsCount1 eAllHeads |> should equal 0.0
   headsCount1 (forceEParts 2 [|true|] eAllTails) |> should equal 99.0

[<Fact>]
let test_rvNStock () =
   ignore (rvNStock : double -> double -> double -> rvseq)
   let rnVStock1 = rvNStock 1.004 0.999 1.0 1
   rnVStock1 eAllHeads |> should equal 1.004
   rnVStock1 eAllTails |> should equal 0.999
   let rnVStock2 = rvNStock 2.0 1.0 1.0 10
   rnVStock2 eAllHeads |> should equal (1.0 * 2.0 ** 10.0)
   rnVStock2 eAllTails |> should equal (1.0 * 1.0 ** 10.0)

   let forceArray = [|true; true; true; true; false; 
                      false; false; false; false; false|]
   rnVStock2 (forceEParts 0 forceArray eAllTails) 
      |> should equal (1.0 * 2.0 ** 4.0)

[<Fact>]
let tset_rvPathD () =
   ignore (rvPathD : rvseq)
   let forceArray = [|false; false; true; false; true; true; false|]
   let rvPathD1 = rvPathD 6
   let rvPathD2 = rvPathD 5
   let rvPathD3 = rvPathD 4
   let eventGen = (forceEParts 0 forceArray eAllTails)
   rvPathD1 eventGen |> should equal 0.0
   rvPathD2 eventGen |> should equal 20.0
   rvPathD3 eventGen |> should equal 10.0
   

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

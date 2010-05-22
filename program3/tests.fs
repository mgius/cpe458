(* Program 3: Testing Suite
   Mark Gius
 *)
#light
open Xunit
open FsxUnit.Syntax
open System

open Ass3


[<Fact>]
let test_deriv () =
   let poly1 = [(1.0,2.0)]
   let poly1deriv = [(2.0,1.0)]
   (deriv poly1) |> should equal poly1deriv

   let poly2 = [(1.0,2.0);(2.0,1.0);(3.0,0.0)]
   let poly2deriv = [(2.0,1.0);(2.0,0.0)]
   (deriv poly2) |> should equal poly2deriv

   let poly3 = [(3.0,0.0)]
   let poly3deriv = []
   (deriv poly3) |> should equal poly3deriv

[<Fact>]
let test_evalPoly () =
   let poly1 = [(1.0,2.0)]
   (evalPoly poly1 1.0) |> should equal 1.0
   (evalPoly poly1 2.0) |> should equal 4.0
   (evalPoly poly1 -4.0) |> should equal 16.0
   
   let poly2 = [(1.0,2.0);(2.0,1.0);(3.0,0.0)]
   (evalPoly poly2 1.0) |> should equal 6.0
   (evalPoly poly2 2.0) |> should equal 11.0

[<Fact>]
let test_findZeros () =
   let nearlyEqual = equalwithtolerance 0.001
   let poly1 = [(1.0,2.0)]
   (findZeros (poly1, 0.0)) |> should equal 0.0
   (nearlyEqual (evalPoly poly1 (findZeros (poly1, 1.0))) 0.0 ) 
      |> should equal true
   
   (almostEqual (evalPoly poly1 (findZeros (poly1, 1000.0))) 0.0) 
      |> should equal true

   let poly2 = [(1.0,2.0);(-4.0,1.0);(3.0,0.0)]

   (almostEqual (evalPoly poly2 (findZeros (poly2, 6.0))) 0.0) 
      |> should equal true
   (almostEqual (evalPoly poly2 (findZeros (poly2, 2.5))) 0.0) 
      |> should equal true
   (almostEqual (evalPoly poly2 (findZeros (poly2, 1.5))) 0.0) 
      |> should equal true
   (almostEqual (evalPoly poly2 (findZeros (poly2, 0.0))) 0.0) 
      |> should equal true
   let poly3 = [(3.0,0.0)]
   (findZeros (poly3, 0.0)) |> should equal 0.0 
   (findZeros (poly3, 1.0)) |> should equal 1.0

[<Fact>]
let test_updateTree () =
   // I wrote this monstrosity, I better test it
   let tree1 = Undef // empty tree
   let flips1 = [AFlip(true); AFlip(true)]
   let value1 = 30.0
   let probability = 0.5 
   let result1 = Node(Node(Leaf 7.5, Undef), Undef)
   let tree2 = updateTree probability flips1 value1 tree1 
   tree2 |> should equal result1

   let flips2 = [AFlip(true); AFlip(false); AFlip(true)]
   let value2 = 40.0
   let result2 = Node( Node(Leaf 7.5, 
                            Node(Leaf 5.0, Undef)),
                       Undef)
   let tree3 = updateTree probability flips2 value2 tree2
   tree3 |> should equal result2

   // Testing Unobserved
   let flips3 = [AFlip(false); Unobserved; AFlip(true)]
   let value3 = 80.0
   let result3 = Node( Node(Leaf 7.5, 
                            Node(Leaf 5.0, Undef)),
                       Node( Node( Leaf 10.0, Undef),
                             Node( Leaf 10.0, Undef)))
   let tree4 = updateTree probability flips3 value3 tree3
   tree4 |> should equal result3

   let flips4 = [AFlip(false); Unobserved; AFlip(false)]
   let value4 = 160.0
   let result4 = Node( Node(Leaf 7.5, 
                            Node(Leaf 5.0, Undef)),
                       Node( Node( Leaf 10.0, Leaf 20.0),
                             Node( Leaf 10.0, Leaf 20.0)))

   let tree5 = updateTree probability flips4 value4 tree4
   tree5 |> should equal result4

   let flips5 = [AFlip(true); AFlip(false); AFlip(false)]
   let value5 = 200.0
   let result5 = Node( Node(Leaf 7.5, 
                            Node(Leaf 5.0, Leaf 25.0)),
                       Node( Node( Leaf 10.0, Leaf 20.0),
                             Node( Leaf 10.0, Leaf 20.0)))
   let tree6 = updateTree probability flips5 value5 tree5
   tree6 |> should equal result5

[<Fact>]
let test_forceEParts () =
   let makeERandP p = 
      let mapRef = ref (Map.empty : Map<int, event1>)
      let rand = System.Random()
      let eRandom timeStep =
         match ((!mapRef).TryFind timeStep) with 
            | Some(boolean) -> boolean
            | None -> let result = if rand.NextDouble() <= p then true else false
                      mapRef := (!mapRef).Add (timeStep, result)
                      result
   
      eRandom 
   (* Create a random variable, call it a number of times, then
      overlay an array and check to see if it took hold *)
   let eRandom1 = makeERandP 0.5 
   (eRandom1 1) |> should equal (eRandom1 1)
   let array1 = [|true; false; true; true; false;|]
   for i = 1 to 10 do ignore (eRandom1 i)
   let eRandom2 = forceEParts 2 array1 eRandom1
   for i = 2 to 6 do (eRandom2 i) |> should equal array1.[i-2]
   (* Test for bug of mine.  Wasn't handling timestep <> the overlay start *)
   (eRandom2 1) |> should equal (eRandom2 1)
   (eRandom2 7) |> should equal (eRandom2 7)

   (* Create a random variable, grab a result, invert it, and overlay it *)
   let eRandom3 = makeERandP 0.5
   let result = eRandom3 1
   let eRandom4 = forceEParts 1 [|not result|] eRandom3
   result |> should not_equal (eRandom4 1)

   (* Original has not changed *)
   result |> should equal (eRandom3 1)


[<Fact>]
let test_generateFlips () =
   let set1 = Set.empty.Add(1).Add(3).Add(4)
   let anEvent1 = forceEParts 1 [|true;false;false;true|] eAllHeads
   let targetFlips1 = [AFlip(true);Unobserved;AFlip(false);AFlip(true)]
   generateFlips set1 anEvent1 |> should equal targetFlips1


[<Fact>]
let test_findUndef () =
   let tree1 = Node( Leaf 1.0, Undef)
   let undef1 = [false]
   findUndef tree1 |> should equal undef1

   let tree2 = Node( Node ( Leaf 1.0, 
                            Node (Undef, 
                                  Undef)),
                     Undef)
   let undef2 = [true;false;true]
   findUndef tree2 |> should equal undef2

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
   let probability1 = 0.5 
   let result1 = Node(Node(Leaf 7.5, Undef), Undef)

   updateTree probability1 flips1 value1 tree1 |> should equal result1

   updateTree 


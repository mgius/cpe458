(* Program 3: Testing Suite
   Mark Gius
 *)
#light
open Xunit
open FsxUnit.Syntax
open System

open program3

let equalWithTolerance tolerance left right =
   (abs (right - left)) < tolerance

[<Fact>]
let test_deriv () =
   let poly1 = [(1.0,2.0)]
   let poly1deriv = [(2.0,1.0)]
   (deriv poly1) |> should equal poly1deriv

   let poly2 = [(1.0,2.0);(2.0,1.0);(3.0,0.0)]
   let poly2deriv = [(2.0,1.0);(2.0,0.0)]
   (deriv poly2) |> should equal poly2deriv

[<Fact>]
let test_evalPoly () =
   let poly1 = [(1.0,2.0)]
   (evalPoly poly1 1.0) |> should equal 1.0
   (evalPoly poly1 2.0) |> should equal 4.0
   (evalPoly poly1 -4.0) |> should equal 16.0
   
   let poly2 = [(1.0,2.0);(2.0,1.0);(3.0,0.0)]
   (evalPoly poly2 1.0) |> should equal 6.0
   (evalPoly poly2 2.0) |> should equal 11.0

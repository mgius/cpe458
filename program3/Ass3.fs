#light
module Ass3 

type term = (double * double)
type poly = term list


(* generic comparison that compares two numbers with some tolerance *)
let equalwithtolerance tolerance val1 val2 =
  (abs (val2 - val1)) < tolerance 

(* specific instantiation of equalwithtolerance for use in testing *)
let almostEqual = equalwithtolerance 0.000001

(* computes the derivative of a polynomial, returning the derivative
   as another polynomial.  Taking the polynomial of a x^0 term results in
   an empty polynomial
 *)
let deriv (terms : poly) =
   let singlederiv (a, b) =
      if b = 0.0 then [] else [(a * b, b - 1.0)]
   let rec loop (acc : poly) = function
      | [] -> acc
      | hd :: tl -> 
         loop (acc @ (singlederiv hd)) tl
   loop [] terms

(* evaluates a given polynomial at x by applying the value to each
   term in the polynomial
 *)
let evalPoly (eqn : poly) x =
   List.fold (fun acc (a,b) -> acc + a * (x ** b)) 0.0 eqn

(* given this equation and starting point, find _a_ point where
   the equation is zero using netwon raphsen *)
let findZeros (eqn, start) =
   let eqnDeriv = deriv eqn
   // check for bad derivative
   if eqnDeriv = [] 
   then 
      (* I'd love to return some sort of error code, but I can only
         return a double, and any double is a valid answer.
         I suppose I could do another "wrap" and return a tuple
         to signal the result along with the status, but that's just
         silly.  I'll be content with a print statement
       *)
      printfn "The equation passed in has a zero derivative"
      start
   else
      let rec inner iterations x =
         if (almostEqual (evalPoly eqn x) 0.0) || iterations < 1
         then x 
         else
            inner (iterations - 1)
                  (x - (evalPoly eqn x) / 
                       (evalPoly eqnDeriv x))
      inner 1000 start

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

type event1 = bool
type event = int -> event1
type rv = event -> double // a random variable

(* DataType to keep track of an events "flips" 
   Since we can have unobserved flips on the way to the final flip
   we must have a representation for them *)
type flip =
   | AFlip of event1
   | Unobserved

(* tree structure to represent the values I've explored 
   I'm defining the "Left" branch as the "true" branch
 *)

type tree =
   | Undef // Unexplored branch
   | Leaf of float // value of this branch
   | Node of tree * tree // children of this branch

(* updates the tree.  Somewhat adapted from http://ky13.net/0gKB 
   Given a list of flips, update the appropriate branches in the tree
   to reflect this
   function maps on the flip list * rest of the tree
 *)
let updateTree probability flips initialValue aTree =
   let rec inner value = function
      // Base case, assign value to a Leaf
      | [], Undef -> Leaf value 
      // I've reached an unobserved point, and the branch is undefined.
      // Define it, and follow both branches
      | Unobserved :: tl , Node(left, right) -> 
         Node(inner (value * probability) (tl, left),
              inner (value * (1.0 - probability)) (tl, right))
      // Same as above except the tree is undefined below here
      | Unobserved :: tl , Undef ->
         Node(inner (value * probability) (tl, Undef),
              inner (value * (1.0 - probability)) (tl, Undef))
      // we have a specific flip, so we want to descend only on that 
      // side of the tree
      | AFlip(result) :: tl , Node(left, right) ->
         if result 
         then 
            Node(inner (value * probability) (tl, left), right)
         else
            Node(left, inner (value * (1.0 - probability)) (tl, right))
      // Same as above except the tree is undefined below here
      | AFlip(result) :: tl , Undef ->
         if result
         then
            Node(inner (value * probability) (tl, Undef), Undef)
         else
            Node(Undef, inner (value * (1.0 - probability)) (tl, Undef))
             
      // The following cases should never happen, but are necessary to 
      // make pattern matching complete
      | [], _ -> Leaf -1.0  // Other logic prevents hitting the same leaf
      | _ , Leaf(v) -> Leaf v // This also shouldn't happen...
   inner initialValue (flips, aTree)


(* left-depth first search looking for undefined branches
   Returns the path to the undefined branch
 *)
let findUndef treeRoot =
   let rec inner direction = function
      | Leaf(i) -> []
      | Undef -> [direction]
      | Node(leftN, rightN) ->
         match inner true leftN with // left branch
            | [] -> 
               match inner false rightN with 
                  | [] -> []
                  | path -> [direction] @ path
            | path -> [direction] @ path

   match treeRoot with 
      | Leaf(i) -> [] // This really shouldn't happen...
      | Undef -> [] // This is possible, but unlikely
      | Node(leftN, rightN) -> 
         let leftPath = inner true leftN
         if not (leftPath = []) then leftPath 
         else
            inner false rightN
            
(* Given a set of timesteps and an Event, construct a 
   well structured set of flips for use by the tree updater 
 *)
let generateFlips aSet anEvent =
   let rec inner acc count innerSet =
      if Set.count innerSet = 0 then acc // base case
      else if Set.minElement innerSet = count then 
         inner (acc @ [AFlip(anEvent count)]) 
               (count + 1) 
               (Set.remove count innerSet)
      else
         inner (acc @ [Unobserved]) (count + 1) innerSet
   inner [] 1 aSet
      

(* Records the timeSteps called by an Event *)
//let eventRecorder someSet anEvent i =
//   anEvent i

(* wrap an event in a set series of results 
   Copied from program2 
 *)
let forceEParts t ba anEvent timeStep =
   if timeStep >= t + (Array.length ba) || timeStep < t
      then (anEvent timeStep)
      else 
         ba.[timeStep - t]

let eAllHeads timeStep = true

let sneakyEvent anEvent (setRef : Set<int> ref) timeStep =
   setRef := (!setRef).Add(timeStep)
   anEvent 1

let expectedVal (randomV : rv) (headP : double) =
   1.0

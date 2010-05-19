module program3 

type term = (double * double)
type eqn = term list

let deriv (terms : eqn) =
   let singlederiv (a, b) =
      if b = 0.0 then [] else [(a * b, b - 1.0)]
   let rec loop (acc : eqn) = function
      | [] -> acc
      | hd :: tl -> 
         loop (acc @ (singlederiv hd)) tl
   loop [] terms

let evalPoly (poly : eqn) x =
   List.fold (fun acc (a,b) -> acc + a * (x ** b)) 0.0 poly

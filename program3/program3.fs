module program3 

type term = (double * double)
type poly = term list

let deriv (terms : poly) =
   let singlederiv (a, b) =
      if b = 0.0 then [] else [(a * b, b - 1.0)]
   let rec loop (acc : poly) = function
      | [] -> acc
      | hd :: tl -> 
         loop (acc @ (singlederiv hd)) tl
   loop [] terms

let evalPoly (eqn : poly) x =
   List.fold (fun acc (a,b) -> acc + a * (x ** b)) 0.0 eqn

let findZeros (eqn, x) =
   

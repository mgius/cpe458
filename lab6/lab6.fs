#light

let putoption strike final = max 0.0 (strike - final)
let calloption strike final = max 0.0 (final - strike)

let nchoosek (n : int) k = 
   if n < k then 0
   else
      let newK = if k > (n / 2) then n - k else k
      let myS = seq { for i in 1..newK -> (n-(i-1), i) } 
      Seq.fold (fun acc x -> acc * fst x / snd x ) 1 myS

let optionValue (periods : int) (s0 : double) 
                (u : double) (d : double) (r : double) 
                (opt : double -> double) =
   let p = (r - d) / (u - d)
   let q = 1.0 - p

   let myS = seq { 0..periods }
   let left iter = (p ** iter) * (q ** ((double periods) - iter))
   let right iter = (opt (s0 * (u ** iter) * (d ** ((double periods) - iter))))
   let sum = Seq.fold 
              (fun acc x -> 
               (left (double x)) * 
               (double (nchoosek periods x)) * 
               (right (double x)) + 
               acc)
              0.0 myS
   
   ((1.0 / r) ** (double periods)) * sum

optionValue 2 75.0 (6.0 / 5.0) (4.0 / 5.0) (11.0 / 10.0) (putoption 95.0)
   |> printfn "%f"

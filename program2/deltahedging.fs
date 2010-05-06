(* Program 2: Delta Hedgin
   Mark Gius
   Collaboration with Jesse Tyler
 *)
#light
module deltahedging

type event1 = bool
type event = int -> event1
type rv = event -> double
type rvseq = int -> rv
type option = double -> double
type stockmodel = (double * double * double * double) // u, d, 1+r, and S_0

let eAllHeads timeStep = 
   true

let eAllTails timeStep =
   false

let eAlternating timeStep =
   if (timeStep % 2 = 0) then true else false

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

let makeERandom () = makeERandP 0.5

let makeERandT () = 
   let mapRef = ref (Map.empty : Map<int, event1>)
   let rand = System.Random()
   let lastResult = ref ( rand.NextDouble() <= 0.5 )
   let eRandom timeStep =
      match ((!mapRef).TryFind timeStep) with 
         | Some(boolean) -> boolean
         | None -> let result = if rand.NextDouble() <= 0.75 
                                  then !lastResult 
                                  else lastResult := not !lastResult
                                       !lastResult
                   mapRef := (!mapRef).Add (timeStep, result)
                   result

   eRandom 

let forceEParts t ba anEvent =
   let eRandom timeStep =
      if timeStep >= t + (Array.length ba) || timeStep < t
        then (anEvent timeStep)
        else 
         ba.[timeStep - t]
   eRandom

let doubleToRV f = 
   let randomV ev =
      f
   randomV


let rvNCountSomething something i = 
   let randomV (ev : event) = 
      let results = seq { for j in 0..(i-1) -> (ev j) }
      //let aggregate = Seq.countBy (fun elem -> elem) results
      //match Seq.tryFind (fun elem -> something = (fst elem)) aggregate with
      //   | Some((_, count)) -> double count
      //   | None -> 0.0
      double (Seq.fold (fun acc x -> if x = something then acc + 1 else acc) 
                       0 results)
   randomV

let rvNCountHeads = rvNCountSomething true 
let rvNCountTails = rvNCountSomething false 

let rvNStock (u : float) (d : float) (initial : float) timeSteps =
   let randomV (ev : event) =
      let results = seq { for i in 1..(timeSteps) -> (ev i)}
      if timeSteps = 0 then initial else 
         double (Seq.fold (fun acc x -> if x then acc * u else acc * d) 
                           initial results)
   randomV


let rvPathD timeStep = 
   let randomV (ev : event) = 
      let results = seq { for i in (timeStep%3)..3..(timeStep-3) -> (ev i) }
      double (Seq.fold (fun acc x -> if x then acc + 10 else acc) 
                       0 results)
   randomV

let unaryLiftRV doubleFunc randVar =
   let randomV (ev : event) =
      doubleFunc (randVar ev)
   randomV

let binaryLiftRV doubleFunc randVarA randVarB =
   let randomV (ev : event) =
      doubleFunc (randVarA ev) (randVarB ev)
   randomV

let putOptionPayoff strike stock = 
   max 0.0 (strike - stock)

let callOptionPayoff strike stock = 
   max 0.0 (stock - strike)

let nchoosek (n : int) k = 
   if n < k then 0
   else
      let newK = if k > (n / 2) then n - k else k
      let myS = seq { for i in 1..newK -> (n-(i-1), i) } 
      Seq.fold (fun acc x -> acc * fst x / snd x ) 1 myS

let optionValueHelper (periods : int) (s0 : double) 
                (u : double) (d : double) (r : double) 
                (opt : double -> double) =
   let p = (r - d) / (u - d)
   let q = 1.0 - p

   let myS = seq { 0..periods }
   let left iter = (p ** iter) * (q ** ((double periods) - iter))
   let right iter = opt (s0 * (u ** iter) * (d ** ((double periods) - iter)))
   let sum = Seq.fold 
              (fun acc x -> 
               (left (double x)) * 
               (double (nchoosek periods x)) * 
               (right (double x)) + 
               acc)
              0.0 myS
   
   ((1.0 / r) ** (double periods)) * sum

let optionValue stockmodel timeperiods anOption =
   let (u, d, oneplusr, initial) = stockmodel
   let randomVSeq i (ev : event) =
      let currentValue = rvNStock u d initial i ev
      optionValueHelper (timeperiods - i) currentValue u d oneplusr anOption
   randomVSeq

let delta stockValues optionValues i (ev : event) =
   let nextTailsEvent = forceEParts (i+1) [|false|] ev
   let nextHeadsEvent = forceEParts (i+1) [|true|] ev

   let top = (optionValues (i+1) nextHeadsEvent) -
             (optionValues (i+1) nextTailsEvent)
   let bottom = (stockValues (i+1) nextHeadsEvent) -
                (stockValues (i+1) nextTailsEvent)
   -(top / bottom)

let initial, u, d, r = (75.0, 3.0, 1.0, 2.0)

let optionValue1 = optionValue (u, d, r, initial) 2 (putOptionPayoff 95.0)
let stockValue1 = rvNStock u d initial
let event1 = forceEParts 0 [|true; true;|] eAllTails
let event2 = forceEParts 0 [|true; false;|] eAllTails
let event3 = forceEParts 0 [|false; false;|] eAllTails

printfn "%f" (delta stockValue1 optionValue1 0 event1)
printfn "%f" (delta stockValue1 optionValue1 1 event1)
printfn "%f" (delta stockValue1 optionValue1 0 event2)
printfn "%f" (delta stockValue1 optionValue1 1 event2)
printfn "%f" (delta stockValue1 optionValue1 0 event3)
printfn "%f" (delta stockValue1 optionValue1 1 event3)

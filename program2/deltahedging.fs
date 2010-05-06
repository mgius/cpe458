(* Program 2: Delta Hedgin
   Mark Gius
   Collaboration with Jesse Tyler
 *)
#light
module deltahedging


(* types from assignment page *)
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

(* Generate true false events with true likelihood of P
   Uses a reference and a map to keep track of already
   generated events
 *)
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

(* SubSet of makeERandP *)
let makeERandom () = makeERandP 0.5

(* Trending is awesome *)
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

(* wrap an event in a set series of results *)
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

(* Master function for counting heads or tails *)
let rvNCountSomething something i = 
   let randomV (ev : event) = 
      let results = seq { for j in 0..(i-1) -> (ev j) }
      double (Seq.fold (fun acc x -> if x = something then acc + 1 else acc) 
                       0 results)
   randomV

let rvNCountHeads = rvNCountSomething true 
let rvNCountTails = rvNCountSomething false 

(* Calcs value of stock after timesteps *)
let rvNStock (u : float) (d : float) (initial : float) timeSteps =
   let randomV (ev : event) =
      let results = seq { for i in 1..(timeSteps) -> (ev i)}
      if timeSteps = 0 then initial else 
         double (Seq.fold (fun acc x -> if x then acc * u else acc * d) 
                           initial results)
   randomV


(* I have no idea why this function is useful *)
let rvPathD timeStep = 
   let randomV (ev : event) = 
      let results = seq { for i in (timeStep%3)..3..(timeStep-3) -> (ev i) }
      double (Seq.fold (fun acc x -> if x then acc + 10 else acc) 
                       0 results)
   randomV

(* These are probably useful for illustration, but I don't know
   how to make it work 
 *)
let unaryLiftRV doubleFunc randVar =
   let randomV (ev : event) =
      doubleFunc (randVar ev)
   randomV

let binaryLiftRV doubleFunc randVarA randVarB =
   let randomV (ev : event) =
      doubleFunc (randVarA ev) (randVarB ev)
   randomV


(* The following four functions ripped from lab6 *)
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

(* Gives the expected value of the option after a number of periods *)
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

(* Gives the expected value of the option given a starting time and
   a particular option and stockmodel
 *)
let optionValue stockmodel timeperiods anOption =
   let (u, d, oneplusr, initial) = stockmodel
   let randomVSeq i (ev : event) =
      let currentValue = rvNStock u d initial i ev
      optionValueHelper (timeperiods - i) currentValue u d oneplusr anOption
   randomVSeq

(* The equation from Clements / Class
   According to wikipedia, this number represents a percentage of the 
   number of stocks that we should be buying / selling to hedge.
   So if we get .50 out of this and our option is for 100 stocks, we should
   buy/sell 50 stocks to hedge
 *)
let delta stockValues optionValues i (ev : event) =
   let nextTailsEvent = forceEParts (i+1) [|false|] ev
   let nextHeadsEvent = forceEParts (i+1) [|true|] ev

   let top = (optionValues (i+1) nextHeadsEvent) -
             (optionValues (i+1) nextTailsEvent)
   let bottom = (stockValues (i+1) nextHeadsEvent) -
                (stockValues (i+1) nextTailsEvent)
   -(top / bottom)

(* We are punting here.  We lack the knowledge about delta hedging to
   correctly interpret the output of delta to make this function work 
   properly.  Unless this is actually correct, in which case disregard
   the previous statement.
 *)
let illustration model timesteps anOption (ev : event) : unit =
   let (u, d, oneplusr, initial) = model

   let stockValues = rvNStock u d initial
   let optionValues = optionValue model timesteps anOption

   for i in 0..(timesteps-1) do
      let thisDelta = delta stockValues optionValues i ev
      if thisDelta = 0.0 then
         printfn "Step %d, Do Nothing." i
      else if thisDelta > 0.0 then
         printfn "Step %d, Short %f of the stock" i thisDelta
      else
         printfn "Step %d, Buy %f of the stock" i (abs thisDelta)
//illustration (2.0, 0.5, 1.1, 100.0) 3 (callOptionPayoff 190.0) 
//   (forceEParts 1 [|true; true; false|] eAllTails)

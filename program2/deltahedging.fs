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
      let results = seq { for i in 0..(timeSteps-1) -> (ev i)}
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

#light
module deltahedging

type event1 = bool

type event = int -> event1

let eAllHeads timeStep = 
   (true : event1)

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
   let lastResult = ref ( true )
   let rand = System.Random()
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

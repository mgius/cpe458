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

//let ref v = { contents = v }      (* val ref  : 'a -> 'a ref *)
//let (!) r = r.contents            (* val (!)  : 'a ref -> 'a *)
//let (:=) r v = r.contents <- v    (* val (:=) : 'a ref -> 'a -> unit *)

let makeERandom () =
   let mapRef = ref (Map.empty : Map<int, event1>)
   let rand = System.Random()
   let eRandom timeStep =
      match ((!mapRef).TryFind timeStep) with 
         | Some(boolean) -> boolean
         | None -> let result = if rand.Next(2) = 0 then true else false
                   mapRef := (!mapRef).Add (timeStep, result)
                   result

   eRandom 

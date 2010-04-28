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

#light
module deltahedging

type event1 = bool

type event = int -> event1

let eAllHeads timeStep = 
   (true : event1)

let eAllTails timeStep =
   false



(* These calls verify the types of the functions to handin *)
ignore (eAllHeads : event)
ignore (eAllTails : event)
//ignore (eAlternating : event)
//ignore (makeERandom : unit -> event)
//ignore (makeERandP : double -> event)
//ignore (makeERandT : unit -> event)
//ignore (forceEParts : int -> bool array -> event -> event)
//ignore (doubleToRV : double -> rv)
//ignore (rvNCountHeads : int -> rv)
//ignore (rvNCountTails : int -> rv)
//ignore (rvNStock : double -> double -> double -> rvseq)
//ignore (rvPathD : rvseq)
//ignore (unaryLiftRV : (double -> double) -> rv -> rv)
//ignore (binaryLiftRV : (double -> double -> double) -> rv -> rv -> rv)
//ignore (putOptionPayoff : double -> option)
//ignore (callOptionPayoff : double -> option)
//ignore (tabulateN : (int -> 'a) -> int -> 'a list)
//ignore (mean : double list -> double)
//ignore (sampleVar : double list -> double)
//ignore (sampleHeads : int -> double)
//ignore (sampleHeadsMeanAndVariance : int -> int -> (double * double))
//ignore (experiment1 : (double * double) list)

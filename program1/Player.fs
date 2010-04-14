#light

module Player

let gameBoard pos = 
    match pos with
        | 1 -> 38
        | 4 -> 14
        | 9 -> 31
        | 16 -> 6
        | 21 -> 42
        | 28 -> 84
        | 36 -> 44
        | 47 -> 26
        | 49 -> 11
        | 51 -> 67
        | 56 -> 53
        | 62 -> 19
        | 64 -> 60
        | 71 -> 91
        | 80 -> 100
        | 87 -> 24
        | 93 -> 73
        | 96 -> 75
        | 98 -> 78
        | _ -> pos

[<AbstractClass>]
type Player() =
    abstract shouldDouble : int -> int -> bool
    abstract shouldTake : int -> int -> bool

(* This player always doubles if he is ahead, and always takes a double,
   regardless of his position
 *)
type RecklessPlayer() =
    inherit Player()
    override this.shouldDouble myPos hisPos =
        myPos > hisPos 

    override this.shouldTake myPos hisPos =
        true

(* 
   Slightly more conservative playeri

   Takes bets if he's not too far behind
   Offers bets if he's far enough ahead
*)
type ConservativePlayer() =
    inherit Player()
    override this.shouldDouble myPos hisPos =
        myPos > hisPos + 10

    override this.shouldTake myPos hisPos =
        myPos > hisPos + 10

type Gius() =
    inherit Player()

    // Averages the positions that can be achived in the next 2 turns
    let score1 pos = 
        float (List.fold (fun acc x -> acc + x) 0
                   [for j in [for i in 1..6 -> gameBoard (pos + i)]
                       -> gameBoard j]) / 6.0

    override this.shouldDouble myPos hisPos = 
//        printfn "DEBUG: I'm at %d, he's at %d, %f vs %f" 
//            myPos hisPos (score1 myPos) (score1 hisPos)
        score1 myPos > score1 hisPos 
    override this.shouldTake myPos hisPos = 
        true

(*
    This player prolongs the game by always accepting bets, but never
    offering them
*)
type ProlongingPlayer() =
    inherit Player()
    override this.shouldDouble myPos hisPos =
        false

    override this.shouldTake myPos hisPos =
        true

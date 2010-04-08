#light

let die = System.Random()

let roll () = die.Next(1,7)

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
    let mutable _pos = 0

    member this.pos
        with get() = 
            _pos
        and set(newValue) =
            _pos <- newValue 

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

type Gius() =
    inherit Player()
    override this.shouldDouble myPos hisPos = 
        true
    override this.shouldTake myPos hisPos = 
        true

let rec realRunGame (playerOne : Player) playerTwo whoseTurn bet cubeOwner =
    match gameBoard (playerOne.pos + (roll ())) with
        | newPos when newPos > 100 ->
            whoseTurn % 2 + 1 |> printfn "Player %d has won!"
        | newPos -> 
            playerOne.pos <- playerOne.pos + newPos
            realRunGame playerTwo playerOne (whoseTurn + 1) bet cubeOwner

//realRunGame (Gius()) (Gius()) 0 0 1 -1

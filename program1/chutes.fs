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
    override this.shouldDouble myPos hisPos = 
        true
    override this.shouldTake myPos hisPos = 
        true

let rec realRunGame (playerOne : Player) (playerTwo : Player) whoseTurn doubles cubeOwner =
    let newDoubles = 
        match playerOne.shouldDouble playerOne.pos playerTwo.pos with
            | true -> 
                match playerOne.shouldTake playerOne.pos playerTwo.pos with
                    | true -> doubles + 1
                    | false -> doubles - 1
            | false -> doubles

    let playerMod = 
        match whoseTurn % 2 with
            | 0 -> 1.0
            | 1 -> -1.0
            | _ -> 0.0 // impossible
        
    match newDoubles, gameBoard (playerOne.pos + (roll ())) with
        | newDoubles, _ when newDoubles < doubles ->
            // This is an indicator that one of the players rejected the bet
            2.0 ** float doubles * playerMod
        | _, newPos when newPos > 100 ->
            2.0 ** float newDoubles * playerMod
        | _, newPos -> 
            playerOne.pos <- playerOne.pos + newPos
            realRunGame playerTwo playerOne (whoseTurn + 1) newDoubles cubeOwner

realRunGame (RecklessPlayer()) (ConservativePlayer()) 0 0 -1 |> printfn "%f"

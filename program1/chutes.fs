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

let rec realRunGame gameState 
                    (playerOne : Player) (playerTwo : Player) =

    let (whoseTurn, doubles, cubeOwner, pos1, pos2) = gameState
    // Figure out what the new doubling value is

    let newDoubles, newOwner = 
        match (cubeOwner = -1 || cubeOwner % 2 = whoseTurn % 2,
               playerOne.shouldDouble pos1 pos2) with
            | (true, true) -> 
                match playerTwo.shouldTake pos2 pos1 with
                    | true -> (doubles + 1, whoseTurn + 1)
                    | false -> (doubles - 1, cubeOwner)
            // Since we only care about true true, catch-all
            | (_, _) -> (doubles, cubeOwner)

    let playerMod = 
        match whoseTurn % 2 with
            | 0 -> 1.0
            | 1 -> -1.0
            | _ -> 0.0 // impossible
        
    match newDoubles, gameBoard (pos1 + (roll ())) with
        | newDoubles, _ when newDoubles < doubles ->
            // This is an indicator that playertwo rejected the bet
            (pos1, pos2) ||> printfn "Rejected: %d %d"
            2.0 ** float doubles * playerMod
        | _, newPos when newPos >= 100 ->
            (pos1, pos2) ||> printfn "Victory: %d %d"
            2.0 ** float newDoubles * playerMod
        | _, newPos -> 
            (pos1, pos2) ||> printfn "%d %d"
            realRunGame (whoseTurn + 1, newDoubles, newOwner, pos2, newPos)
                        playerTwo playerOne 

let runGame = realRunGame (0,0,-1,0,0)

runGame (RecklessPlayer()) (RecklessPlayer()) |> printfn "%f"

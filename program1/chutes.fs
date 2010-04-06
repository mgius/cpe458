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
type Player =
    abstract shouldDouble : int -> int -> bool
    abstract shouldTake : int -> int -> bool

type Gius() as this =
    let shouldDouble myPos hisPos = 
        true
    let shouldTake myPos hisPos = 
        true
    public val mutable pos = 0
    public val mutable hasCube = true // Both players "start" with the cube

let rec realRunGame (playerOne : Gius) playerTwo whoseTurn bet cubeOwner =
    match gameBoard (playerOne.pos + (roll ())) with
        | newPos when newPos > 100 ->
            whoseTurn % 2 + 1 |> printfn "Player %d has won!"
        | newPos -> 
            playerOne.pos <- playerOne.pos + newPos
            realRunGame playerTwo playerOne (whoseTurn + 1) bet 
                cubeOwner

let runGame playerOne playerTwo = 
    realRunGame playerOne playerTwo 0 0 1 -1

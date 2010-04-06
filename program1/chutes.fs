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

let rec rungame onePos twoPos whoseTurn =
    match gameBoard (onePos + (roll ())) with
        | newPos when newPos > 100 ->
            whoseTurn % 2 + 1 |> printfn "Player %d has won!"
        | newPos -> rungame twoPos newPos (whoseTurn + 1)

rungame 0 0 0

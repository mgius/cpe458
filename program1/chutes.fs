(**
* Chutes and Ladders
*
* @author Nat Welch
* @author Mark Gius
*)
#light

open Player

let die = System.Random()

let roll () = die.Next(1,7)


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
//            (pos1, pos2) ||> printfn "Rejected: %d %d"
            2.0 ** float doubles * playerMod
        | _, newPos when newPos >= 100 ->
//            (pos1, pos2) ||> printfn "Victory: %d %d"
            2.0 ** float newDoubles * playerMod
        | _, newPos -> 
//            (pos1, pos2) ||> printfn "DEBUG: %d %d"
            realRunGame (whoseTurn + 1, newDoubles, newOwner, pos2, newPos)
                         playerTwo playerOne 

let runGame = realRunGame (0,0,-1,0,0)

let sequence = seq { for i in 1..1000 -> (runGame (RecklessPlayer()) (RecklessPlayer())) }
let resultSeq = Seq.countBy (fun elem -> elem) sequence

"Collected results of 1000 runs of two reckless players" |> printfn "%s"
let printSeq seq1 = Seq.iter (printfn "%A ") seq1
printSeq resultSeq

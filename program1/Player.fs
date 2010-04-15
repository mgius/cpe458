(**
* Chutes and Ladders Player definitions
*
* @author Nat Welch
* @author Mark Gius
*)
#light

module Player

[<AbstractClass>]
type Player() =
    abstract shouldDouble : int -> int -> bool
    abstract shouldTake : int -> int -> bool

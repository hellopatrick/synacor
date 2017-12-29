open Core

type t =
  | Halt
  | Set
  | Push
  | Pop
  | Equal 
  | GreaterThan
  | Jump
  | JumpTrue
  | JumpFalse
  | Add
  | Multiply
  | Mod
  | And
  | Or
  | Not
  | Read
  | Write
  | Call
  | Return
  | Out
  | In
  | Noop

let of_int = function
  | 0 -> Some Halt
  | 1 -> Some Set
  | 2 -> Some Push
  | 3 -> Some Pop
  | 4 -> Some Equal
  | 5 -> Some GreaterThan
  | 6 -> Some Jump
  | 7 -> Some JumpTrue
  | 8 -> Some JumpFalse
  | 9 -> Some Add
  | 10 -> Some Multiply
  | 11 -> Some Mod
  | 12 -> Some And
  | 13 -> Some Or
  | 14 -> Some Not
  | 15 -> Some Read
  | 16 -> Some Write
  | 17 -> Some Call
  | 18 -> Some Return
  | 19 -> Some Out
  | 20 -> Some In
  | 21 -> Some Noop
  | _ -> None
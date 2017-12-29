open Core

type t =
  | Halt
  | Set
  | Push
  | Pop
  | Equal | GreaterThan
  | Jump | JumpT | JumpF
  | Add | Multiply | Mod
  | And | Or | Not
  | Read
  | Write
  | Call
  | Return
  | Out
  | In
  | Noop
  | Unknown of int

let of_int = function
  | 0 -> Halt
  | 1 -> Set
  | 2 -> Push
  | 3 -> Pop
  | 4 -> Equal
  | 5 -> GreaterThan
  | 6 -> Jump
  | 7 -> JumpT
  | 8 -> JumpF
  | 9 -> Add
  | 10 -> Multiply
  | 11 -> Mod
  | 12 -> And
  | 13 -> Or
  | 14 -> Not
  | 15 -> Read
  | 16 -> Write
  | 17 -> Call
  | 18 -> Return
  | 19 -> Out
  | 20 -> In
  | 21 -> Noop
  | c -> Unknown c

let arity = function
  | Noop -> 0
  | Set -> 2
  | Push -> 1
  | Pop -> 1
  | Equal -> 3
  | GreaterThan -> 3
  | Jump -> 1
  | JumpT -> 2
  | JumpF -> 2
  | Add -> 3
  | Multiply -> 3
  | Mod -> 3
  | And -> 3
  | Or -> 3
  | Not -> 2
  | Halt -> 0
  | Read -> 2
  | Write -> 2
  | Call -> 1
  | Return -> 0
  | Out -> 1
  | In -> 1
  | Unknown c -> 0

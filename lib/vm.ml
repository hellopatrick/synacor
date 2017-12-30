open Core

let max_u15 = 32768

type state = Run | Halt

type t = {
  memory: Memory.t;
  pc: int;
  stack: int list;
  input_buffer: char list option;
  state: state;
  debug: bool;
}

let create program =
  let memory = Memory.create_and_load program in
  { memory; pc=0; stack=[]; input_buffer=None; state=Run; debug=false; }

let noop t =
  if t.debug then printf "noop\n";
  { t with pc=(t.pc + 1); }

let jump t =
  let pc = Memory.read t.memory (t.pc + 1) in
  if t.debug then printf "jump to %d\n" pc;
  { t with pc; }

let jump_if_true t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let pc = if a > 0 then b else t.pc + 3 in
  if t.debug then printf "jump_if_true %d to %d\n" a b;
  { t with pc; }

let jump_if_false t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let pc = if a = 0 then b else t.pc + 3 in
  if t.debug then printf "jump_if_false %d to %d\n" a b;
  { t with pc; }

let set t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  Memory.set t.memory a b;
  if t.debug then printf "set %d to %d\n" a b;
  { t with pc=(t.pc + 3); }

let push t =
  let v = Memory.read t.memory (t.pc + 1) in
  let stack = v::t.stack in
  if t.debug then printf "push %d to stack\n" v;
  { t with stack; pc=(t.pc + 2); }

let pop t =
  let a = Memory.get t.memory (t.pc + 1) in
  match t.stack with
  | [] -> failwith "stack is empty;"
  | v::stack ->
    Memory.set t.memory a v;
    if t.debug then printf "pop %d from stack to %d\n" v a;
    { t with stack; pc=(t.pc + 2); }

let compute_and_set t f name =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = (f b c) % max_u15 in
  if t.debug then printf "set %d to (%s %d %d) = %d\n" a name b c v;
  Memory.set t.memory a v

let eq t =
  compute_and_set t (fun x y -> if x = y then 1 else 0) "eq";
  { t with pc=(t.pc + 4); }

let gt t =
  compute_and_set t (fun x y -> if x > y then 1 else 0) "gt";
  { t with pc=(t.pc + 4); }

let add t =
  compute_and_set t Int.( + ) "add";
  { t with pc=(t.pc + 4); }

let multiply t =
  compute_and_set t Int.( * ) "mult";
  { t with pc=(t.pc + 4); }

let modulus t =
  compute_and_set t Int.( % ) "mod";
  { t with pc=(t.pc + 4); }

let bit_and t =
  compute_and_set t Int.bit_and "and";
  { t with pc=(t.pc + 4); }

let bit_or t =
  compute_and_set t Int.bit_or "or";
  { t with pc=(t.pc + 4); }

let bit_not t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let v = ((lnot b) land 65535) % 32768 in
  Memory.set t.memory a v;
  if t.debug then printf "set %d to not %d = %d\n" a b v;
  { t with pc=(t.pc + 3); }

let rmem t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) |> Memory.get t.memory in
  Memory.set t.memory a b;
  if t.debug then printf "rmem %d to %d\n" b a;
  { t with pc=(t.pc + 3); }

let wmem t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  Memory.set t.memory a b;
  if t.debug then printf "wmem %d to %d\n" b a;
  { t with pc=(t.pc + 3); }

let call t =
  let pc = Memory.read t.memory (t.pc + 1) in
  let stack = (t.pc + 2)::t.stack in
  if t.debug then printf "call %d\n" pc;
  { t with stack=stack; pc; }

let ret t =
  match t.stack with
  | [] -> { t with state=Halt; }
  | pc::stack ->
    if t.debug then printf "return to %d\n" pc;
    { t with stack; pc; }

let handle_output t =
  let cc = Memory.read t.memory (t.pc + 1) in
  let letter = Char.of_int_exn cc in

  if t.debug then printf "output %d (from %d)\n" cc (t.pc + 1)
  else printf "%c" letter;

  { t with pc=(t.pc + 2); }

let rec read_stdin t =
  printf ">>> "; Out_channel.flush Out_channel.stdout;

  match In_channel.input_line In_channel.stdin with
  | None -> read_stdin t
  | Some "debug" -> read_stdin { t with debug=(not t.debug); }
  | Some line ->
    let input_buffer = String.to_list (line ^ "\n") |> Option.some in
    { t with input_buffer; }

let handle_input t =
  match t.input_buffer with
  | None ->
    read_stdin t
  | Some [] ->
    { t with input_buffer=None; }
  | Some (c::rest) ->
    let ascii = Char.to_int c in
    let a = Memory.get t.memory (t.pc + 1) in
    Memory.set t.memory a ascii;
    if t.debug then printf "input %d to %d\n" ascii a;
    { t with input_buffer=Some rest; pc=(t.pc + 2);}

let halt t =
  if t.debug then printf "halt.\n";
  { t with state=Halt; }

let perform t op =
  match op with
  | Op.Noop -> noop t
  | Op.Set -> set t
  | Op.Push -> push t
  | Op.Pop -> pop t
  | Op.Equal -> eq t
  | Op.GreaterThan -> gt t
  | Op.Jump -> jump t
  | Op.JumpTrue -> jump_if_true t
  | Op.JumpFalse -> jump_if_false t
  | Op.Add -> add t
  | Op.Multiply -> multiply t
  | Op.Mod -> modulus t
  | Op.And -> bit_and t
  | Op.Or -> bit_or t
  | Op.Not -> bit_not t
  | Op.Read -> rmem t
  | Op.Write -> wmem t
  | Op.Call -> call t
  | Op.Return -> ret t
  | Op.Out -> handle_output t
  | Op.In -> handle_input t
  | Op.Halt -> halt t

let next_op t =
  try
    Memory.get t.memory t.pc
    |> Op.of_int
  with _ -> None

let exec t =
  match next_op t with
  | None -> { t with state=Halt; }
  | Some op -> perform t op

let run t =
  let rec step t =
    Out_channel.flush Out_channel.stdout;
    match t.state with
    | Halt -> t
    | Run -> let t = exec t in step t
  in step t
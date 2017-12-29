open Core

let max_u15 = 32768

type state = Run | Halt

type t = {
  memory: Memory.t;
  pc: int;
  stack: int list;
  input_buffer: char list option;
  state: state;
}

let create program =
  let memory = Memory.create_and_load program in
  { memory; pc=0; stack=[]; input_buffer=None; state=Run; }

let noop t = { t with pc=(t.pc + 1); }

let jump t =
  let pc = Memory.read t.memory (t.pc + 1) in
  { t with pc; }

let jump_if_true t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let pc = if a > 0 then b else t.pc + 3 in
  { t with pc; }

let jump_if_false t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let pc = if a = 0 then b else t.pc + 3 in
  { t with pc; }

let set t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  Memory.set t.memory a b;
  { t with pc=(t.pc + 3); }

let push t =
  let v = Memory.read t.memory (t.pc + 1) in
  let stack = v::t.stack in
  { t with stack; pc=(t.pc + 2); }

let pop t =
  let a = Memory.get t.memory (t.pc + 1) in
  match t.stack with
  | [] -> failwith "stack is empty;"
  | v::stack ->
    Memory.set t.memory a v;
    { t with stack; pc=(t.pc + 2); }

let compute_and_set t f =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = (f b c) % max_u15 in
  Memory.set t.memory a v

let eq t =
  compute_and_set t (fun x y -> if x = y then 1 else 0);
  { t with pc=(t.pc + 4); }

let gt t =
  compute_and_set t (fun x y -> if x > y then 1 else 0);
  { t with pc=(t.pc + 4); }

let add t =
  compute_and_set t Int.( + );
  { t with pc=(t.pc + 4); }

let multiply t =
  compute_and_set t Int.( * );
  { t with pc=(t.pc + 4); }

let modulus t =
  compute_and_set t Int.( % );
  { t with pc=(t.pc + 4); }

let bit_and t =
  compute_and_set t Int.bit_and;
  { t with pc=(t.pc + 4); }

let bit_or t =
  compute_and_set t Int.bit_or;
  { t with pc=(t.pc + 4); }

let bit_not t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let v = ((lnot b) land 65535) % 32768 in
  Memory.set t.memory a v;
  { t with pc=(t.pc + 3); }

let rmem t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) |> Memory.get t.memory in
  Memory.set t.memory a b;
  { t with pc=(t.pc + 3); }

let wmem t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  Memory.set t.memory a b;
  { t with pc=(t.pc + 3); }

let call t =
  let pc = Memory.read t.memory (t.pc + 1) in
  let stack = (t.pc + 2)::t.stack in
  { t with stack=stack; pc; }

let ret t =
  match t.stack with
  | [] -> { t with state=Halt; }
  | pc::stack ->
    { t with stack; pc; }

let handle_output t =
  let cc = Memory.read t.memory (t.pc + 1) in
  let letter = Char.of_int_exn cc in
  printf "%c" letter;
  { t with pc=(t.pc + 2); }

let read_stdin () =
  printf ">>> "; Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> None
  | Some line ->
    let chars = String.to_list (line ^ "\n") in
    Some chars

let handle_input t =
  match t.input_buffer with
  | None ->
    let input_buffer = read_stdin () in
    { t with input_buffer; }
  | Some [] ->
    { t with input_buffer=None; }
  | Some (c::rest) ->
    let ascii = Char.to_int c in
    let a = Memory.get t.memory (t.pc + 1) in
    Memory.set t.memory a ascii;
    { t with input_buffer=Some rest; pc=(t.pc + 2);}

let halt t = { t with state=Halt; }

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
    let code = Memory.get t.memory t.pc in
    match Op.of_int code with
    | None ->
      printf "Op code: %02d not implemented" code;
      None
    | Some op -> Some op
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
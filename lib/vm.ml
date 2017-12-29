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

let jump t =
  Memory.read t.memory (t.pc + 1)

let jump_if_not_zero t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  if a > 0 then b else t.pc + 3

let jump_if_zero t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  if a = 0 then b else t.pc + 3

let set t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  Memory.set t.memory a b;
  t

let push t =
  let v = Memory.read t.memory (t.pc + 1) in
  let stack = v::t.stack in
  { t with stack; }

let pop t =
  let a = Memory.get t.memory (t.pc + 1) in
  match t.stack with
  | [] -> failwith "stack is empty;"
  | v::stack ->
    Memory.set t.memory a v;
    { t with stack; }

let eq t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = if b = c then 1 else 0 in
  Memory.set t.memory a v;
  t

let gt t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = if b > c then 1 else 0 in
  Memory.set t.memory a v;
  t

let add t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let sum = (b + c) % max_u15 in
  Memory.set t.memory a sum;
  t

let multiply t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let product = (b * c) % max_u15 in
  Memory.set t.memory a product;
  t

let modulus t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = (b % c) in
  Memory.set t.memory a v;
  t

let bit_and t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = b land c in
  Memory.set t.memory a v;
  t

let bit_or t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2)
  and c = Memory.read t.memory (t.pc + 3) in
  let v = b lor c in
  Memory.set t.memory a v;
  t

let bit_not t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  let v = ((lnot b) land 65535) % 32768 in
  Memory.set t.memory a v;
  t

let rmem t =
  let a = Memory.get t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) |> Memory.get t.memory in
  Memory.set t.memory a b;
  t

let wmem t =
  let a = Memory.read t.memory (t.pc + 1)
  and b = Memory.read t.memory (t.pc + 2) in
  Memory.set t.memory a b;
  t

let call t =
  let stack = (t.pc + 2)::t.stack in
  { t with stack=stack; }

let ret t =
  match t.stack with
  | [] -> { t with state=Halt; }
  | pc::stack ->
    { t with stack; pc; }

let handle_output t =
  let cc = Memory.read t.memory (t.pc + 1) in
  let letter = Char.of_int_exn cc in
  printf "%c" letter;
  t

let read_stdin () =
  printf ">>> "; Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
  | None -> None
  | Some line ->
    let chars = '\n'::String.to_list_rev line in
    Some (List.rev chars)

let handle_input t =
  match t.input_buffer with
  | None ->
    let input_buffer = read_stdin () in
    { t with input_buffer; }
  | Some (c::rest) ->
    let ascii = Char.to_int c in
    let a = Memory.get t.memory (t.pc + 1) in
    Memory.set t.memory a ascii;
    Out_channel.flush Out_channel.stdout;
    { t with input_buffer=Some rest; pc=(t.pc + 2);}
  | Some [] ->
    { t with input_buffer=None; }

let perform t op =
  let arity = Op.arity op in
  match op with
  | Op.Noop ->
    { t with pc=(t.pc + arity + 1); }
  | Op.Set ->
    { (set t) with pc=(t.pc + arity + 1); }
  | Op.Push ->
    { (push t) with pc=(t.pc + arity + 1); }
  | Op.Pop ->
    { (pop t) with pc=(t.pc + arity + 1); }
  | Op.Equal ->
    { (eq t) with pc=(t.pc + arity + 1); }
  | Op.GreaterThan ->
    { (gt t) with pc=(t.pc + arity + 1); }
  | Op.Jump ->
    { t with pc=(jump t); }
  | Op.JumpT ->
    { t with pc=(jump_if_not_zero t); }
  | Op.JumpF ->
    { t with pc=(jump_if_zero t); }
  | Op.Add ->
    { (add t) with pc=(t.pc + arity + 1); }
  | Op.Multiply ->
    { (multiply t) with pc=(t.pc + arity + 1); }
  | Op.Mod ->
    { (modulus t) with pc=(t.pc + arity + 1); }
  | Op.And ->
    { (bit_and t) with pc=(t.pc + arity + 1); }
  | Op.Or ->
    { (bit_or t) with pc=(t.pc + arity + 1); }
  | Op.Not ->
    { (bit_not t) with pc=(t.pc + arity + 1); }
  | Op.Read ->
    { (rmem t) with pc=(t.pc + arity + 1); }
  | Op.Write ->
    { (wmem t) with pc=(t.pc + arity + 1); }
  | Op.Call ->
    { (call t) with pc=(jump t); }
  | Op.Return ->
    ret t
  | Op.Out ->
    { (handle_output t) with pc=(t.pc + arity + 1); }
  | Op.In ->
    handle_input t
  | Op.Halt | Op.Unknown _ ->
    { t with state=Halt; }

let next_op t =
  try
    let code = Memory.read t.memory t.pc in
    match Op.of_int code with
    | Op.Unknown c ->
      printf "Op code: %02d not implemented" c;
      None
    | op -> Some op
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
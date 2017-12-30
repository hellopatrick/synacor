open Core

type t = {
  registers:int array;
  memory:int array;
}
[@@deriving sexp]

let max_mem = 32768

let create_and_load src =
  let registers = Array.create ~len:8 0
  and memory = Array.create ~len:max_mem 0 in
  Array.blit ~src ~src_pos:0 ~dst:memory ~dst_pos:0 ~len:(Array.length src);
  { registers; memory; }

let get t i =
  if i >= max_mem then Array.get t.registers (i - max_mem)
  else Array.get t.memory i

let value t i =
  if i >= max_mem then Array.get t.registers (i - max_mem)
  else i

let read t i =
  get t i |> value t

let set t addr value =
  if addr >= max_mem then Array.set t.registers (addr - max_mem) value
  else Array.set t.memory addr value

let print t i =
  printf "i: %d -> v: %d \n" i (value t i)

let print_registers t =
  List.to_string ~f:Int.to_string (Array.to_list t.registers)
  |> printf "Registers: %s\n";
  List.to_string ~f:Int.to_string (Array.to_list (Array.map ~f:(get t) t.registers))
  |> printf "Values: %s\n";

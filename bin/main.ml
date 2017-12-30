open Core
open Synacor

let read_input file_name =
  let f channel =
    let buffer = Buffer.create 2 in
    let rec aux l =
      match In_channel.input_buffer channel buffer ~len:2 with
      | Some _ ->
        let buf = Buffer.to_bytes buffer in
        let code = Binary_packing.unpack_unsigned_16_little_endian ~buf ~pos:0 in
        Buffer.reset buffer;
        aux (code::l)
      | None -> Array.of_list_rev l
    in aux []
  in
  In_channel.with_file ~binary:true ~f file_name

let _ =
  let instructions = read_input "./challenge.bin" in
  let _res = VM.(create instructions |> run) in
  ()
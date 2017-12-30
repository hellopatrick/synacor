open Core
open Synacor

let read_input file_name =
  let f channel =
    let buf = Buffer.create 2 in
    let rec aux l =
      match In_channel.input_buffer channel buf ~len:2 with
      | Some _ ->
        let contents = Buffer.to_bytes buf in
        let code = Binary_packing.unpack_unsigned_16_little_endian ~buf:contents ~pos:0 in
        Buffer.reset buf;
        aux (code::l)
      | None -> Array.of_list_rev l
    in aux []
  in
  In_channel.with_file ~binary:true ~f file_name

let _ =
  let instructions = read_input "./challenge.bin" in
  let _res = VM.(create instructions |> run) in
  ()
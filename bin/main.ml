open Core
open Synacor

let read_input file_name =
  let f channel =
    let rec aux l =
      let buf = Buffer.create 2 in
      match In_channel.input_buffer channel buf ~len:2 with
      | Some _ ->
        let buf = Buffer.contents buf in
        let code = Binary_packing.unpack_unsigned_16_little_endian ~buf ~pos:0 in
        aux (code::l)
      | None -> l
    in aux []
  in
  In_channel.with_file ~binary:true ~f file_name
  |> Array.of_list_rev

let _ =
  let instructions = read_input "./challenge.bin" in
  let _res = VM.(create instructions |> run) in
  ()
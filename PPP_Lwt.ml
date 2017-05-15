open Lwt

type writer = string -> unit
type reader = int -> int -> string
type 'a printer = writer -> 'a -> unit
type 'a scanner = reader -> int -> ('a * int) option
type 'a t = { printer : 'a printer ;
              scanner : 'a scanner ;
              descr : string }


(* Build a Lwt stream of values from an Lwt_io.input_channel: *)
let of_input_channel ppp ?(buf_capacity=4096) ic =
  let buf = Bytes.create buf_capacity
  and buf_length = ref 0
  and buf_offset = ref 0 (* offset in ic of the first byte of buf *) in
  let rec reader o l =
    assert (l <= buf_capacity) ;
    if o < !buf_offset then invalid_arg "Cannot backtrack that far"
    else if o > !buf_offset + !buf_length then
      (* No real reason not to allow this, but will have to read the skipped bytes *)
      fail Not_implemented
    else (
      let already_read = !buf_offset + !buf_length - o in
      assert (already_read >= 0) ;
      let to_read = max 0 (l - already_read) in
      let read_into =
        if to_read <= buf_capacity - !buf_length then (
          !buf_length
        ) else (
          (* Need to make room *)
          let keep = buf_capacity - to_read in
          let drop = !buf_length - keep in
          Bytes.blit buf drop buf 0 keep ;
          buf_offset := !buf_offset + drop ;
          buf_length := !buf_length - drop ;
          keep
        ) in
      let%lwt read_len = Lwt_io.read_into ic buf read_into to_read in
      buf_length := !buf_length + read_len ;
      (* Printf.eprintf "Buffer: length=%d, offset=%d, content=%S\n%!"
          !buf_length !buf_offset (Bytes.to_string buf) ; *)
      let str_start = o - !buf_offset in
      let str_len = min l (!buf_length - str_start) in
      (* We are allowed to return a smaller string only in case of EOF: *)
      if str_len < l && read_len > 0 then
        reader o l (* Keep reading *)
      else
        Bytes.sub_string buf str_start str_len
    ) in
  ppp.scanner reader

 

(* Version of PPPs that merely blocks on I/O. *)

module P = PPP.Make (
  struct
    type 'a ct = 'a
    let return x = x
    let bind a b = b a

    type input_chan = in_channel
    type output_chan = out_channel
    let stdin = stdin
    let stdout = stdout
    let stderr = stderr
    let write_string = output_string
    let read ic buf ofs len =
      try really_input ic buf ofs len ; len
      with End_of_file -> 0
  end)

module OCaml = PPP_OCaml.Make (P)
module JSON = PPP_JSON.Make (P)
module CSV = PPP_CSV.Make (P)

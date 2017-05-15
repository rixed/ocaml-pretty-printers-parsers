(* Version of PPPs what reads/writes using Lwt_io *)

module P = PPP.Make (
  struct
    type 'a ct = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind

    open Lwt_io
    type input_chan = input_channel
    type output_chan = output_channel
    let stdin = stdin
    let stdout = stdout
    let stderr = stderr
    let write_string = write
    let read = read_into
  end)

module OCaml = PPP_OCaml.Make (P)
module JSON = PPP_JSON.Make (P)
module CSV = PPP_CSV.Make (P)
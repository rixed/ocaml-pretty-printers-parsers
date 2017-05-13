(* This parser just output everything in a sequence of values separated by
 * pipes.  This is obviously not conserving most of structures so suitable only
 * for very simple data. *)
open PPP
(*$inject
  let id x = x
  let of_string = PPP.of_string
  let to_string = PPP.to_string
 *)

(* Import some operators from PPP (infix ops + those used by ppx_ppp) *)
let (++) = (++)
let (-+) = (-+)
let (+-) = (+-)
let (>>:) = (>>:)
let cst = cst
let none = none

let unit : unit t = cst ""

let bool = bool
let int = int
let int32 = int32
let int64 = int64
let uint32 = uint32
let float = float
let string = string

let sep = "|"

let list (ppp : 'a t) : 'a list t =
  seq "list" "" "" sep List.iteri List.rev ppp

let array (ppp : 'a t) : 'a array t =
  seq "array" "" "" sep Array.iteri (fun l -> Array.of_list (List.rev l)) ppp

let tuple_open = ""
let tuple_close = "\n"
let tuple_sep = "|"

let pair (p1 : 'a t) (p2 : 'b t) : ('a * 'b) t =
  p1 +- cst sep ++ p2

let triple (p1 : 'a t) (p2 : 'b t) (p3 : 'c t) : ('a * 'b * 'c) t =
  p1 +- cst sep ++ p2 +- cst sep ++ p3 >>:
    ((fun (v1,v2,v3) -> (v1,v2),v3),
     (fun ((v1,v2),v3) -> v1,v2,v3))

let option ppp = optional ppp

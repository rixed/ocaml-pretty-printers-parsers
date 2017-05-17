(* This parser just output everything in a sequence of values separated by
 * pipes.  This is obviously not conserving most of structures so suitable only
 * for very simple data. *)
include PPP.Ops

let unit : unit PPP.t = cst ""

let tuple_open = ""
let tuple_close = "\n"
let tuple_sep = "|"

let list (ppp : 'a PPP.t) : 'a list PPP.t =
  PPP.seq "list" "" "" tuple_sep List.fold_left List.rev ppp

let array (ppp : 'a PPP.t) : 'a array PPP.t =
  PPP.seq "array" "" "" tuple_sep Array.fold_left (fun l -> Array.of_list (List.rev l)) ppp

let pair (p1 : 'a PPP.t) (p2 : 'b PPP.t) : ('a * 'b) PPP.t =
  p1 +- cst tuple_sep ++ p2

let triple (p1 : 'a PPP.t) (p2 : 'b PPP.t) (p3 : 'c PPP.t) : ('a * 'b * 'c) PPP.t =
  p1 +- cst tuple_sep ++ p2 +- cst tuple_sep ++ p3 >>:
    ((fun (v1,v2,v3) -> (v1,v2),v3),
     (fun ((v1,v2),v3) -> v1,v2,v3))

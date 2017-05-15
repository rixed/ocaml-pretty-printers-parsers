(* This parser just output everything in a sequence of values separated by
 * pipes.  This is obviously not conserving most of structures so suitable only
 * for very simple data. *)
module Make (P : PPP.S) =
struct

  include P.Ops

  let unit : unit P.t = cst ""

  let sep = "|"

  let list (ppp : 'a P.t) : 'a list P.t =
    P.seq "list" "" "" sep List.fold_left List.rev ppp

  let array (ppp : 'a P.t) : 'a array P.t =
    P.seq "array" "" "" sep Array.fold_left (fun l -> Array.of_list (List.rev l)) ppp

  let tuple_open = ""
  let tuple_close = "\n"
  let tuple_sep = "|"

  let pair (p1 : 'a P.t) (p2 : 'b P.t) : ('a * 'b) P.t =
    p1 +- cst sep ++ p2

  let triple (p1 : 'a P.t) (p2 : 'b P.t) (p3 : 'c P.t) : ('a * 'b * 'c) P.t =
    p1 +- cst sep ++ p2 +- cst sep ++ p3 >>:
      ((fun (v1,v2,v3) -> (v1,v2),v3),
       (fun ((v1,v2),v3) -> v1,v2,v3))
end

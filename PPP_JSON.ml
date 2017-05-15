module Make (P : PPP.S) =
struct
  include P.Ops
  (*$inject
    let id x = x
    let of_string = PPP_block.P.of_string
    let to_string = PPP_block.P.to_string
    module P = PPP_block.JSON
   *)
  (*$< P *)

  let unit : unit P.t = cst "null"
  (*$= unit
    (Some ((), 4)) (of_string unit "null" 0)
   *)

  let list (ppp : 'a P.t) : 'a list P.t =
    P.seq "list" "[" "]" "," List.fold_left List.rev ppp

  let array x = list x >>: (Array.to_list, Array.of_list)

  let groupings = [ "{","}" ; "[","]" ]
  let delims = [ "," ; ";" ]

  let record x = P.record "{" "}" ":" "," groupings delims string x
  let (<->) x y = P.sequence "; " x y
  let field ?default name x = P.field ":" ", " ": " ?default name x

  let union x = P.union "{" "}" ":" groupings delims string x
  let (|||) x y = P.alternative " | " x y
  let variant name x = P.variant ":" "" " of " name x

  let pair (p1 : 'a P.t) (p2 : 'b P.t) : ('a * 'b) P.t =
    record (
      field "fst" p1 <->
      field "snd" p2) >>:
    ((fun (v1, v2) -> Some v1, Some v2),
     (function Some v1, Some v2 -> v1, v2
             | _ -> assert false))

  let triple (p1 : 'a P.t) (p2 : 'b P.t) (p3 : 'c P.t) : ('a * 'b * 'c) P.t =
    record (
      field "fst" p1 <->
      field "snd" p2 <->
      field "thrd" p3) >>:
      ((fun (v1,v2,v3) -> Some (Some v1, Some v2), Some v3),
       (function Some (Some v1, Some v2), Some v3 -> v1,v2,v3
               | _ -> assert false))

  let result ok_ppp err_ppp = union (
    variant "Ok" ok_ppp |||
    variant "Err" err_ppp) >>:
    ((function Ok x -> Some x, None
             | Error e -> None, Some e),
     (function Some x, _-> Ok x
             | _, Some e -> Error e
             | _ -> assert false))
  (*$= result & ~printer:id
    "{\"Err\":\"test\"}" (to_string (result int string) (Error "test"))
    "{\"Ok\":42}"        (to_string (result int string) (Ok 42))
   *)

  (*$inject
    let test_id p x =
      let s = to_string p x in
      match of_string p s 0 with
      | Some (x',_) -> if x = x' then true else (Printf.printf "intermediary string: %S\n" s; false)
      | _ -> false
    let test_id_float x =
      let str = to_string float x in
      match of_string float (to_string float x) 0 with
      | Some (x',_) -> abs_float (x -. x') <= 1e-5
      | _ -> false
  *)
  (*$Q & ~count:10
    Q.int (test_id int)
    Q.float (test_id_float)
    Q.(pair printable_string (small_list int)) (test_id (pair string (list int)))
    Q.string (test_id string)
    Q.(array_of_size Gen.small_int (pair (small_list int) bool)) (test_id (array (pair (list int) bool)))
  *)
  (*$>*)
end

include PPP.Ops
(*$inject
  let id x = x
  let of_string = PPP.of_string
  let to_string = PPP.to_string
 *)

let unit : unit PPP.t = cst "null"
(*$= unit
  (Some ((), 4)) (of_string unit "null" 0)
 *)

let char = PPP.char "\""

let option ppp = PPP.option ~placeholder:(cst "null") ppp

let list (ppp : 'a PPP.t) : 'a list PPP.t =
  PPP.seq "list" "[" "]" "," List.fold_left List.rev ppp

let array x = list x >>: (Array.to_list, Array.of_list)

let groupings = [ "{","}" ; "[","]" ]
let delims = [ "," ]

let record x = PPP.record "{" "}" ":" "," groupings delims string x
let (<->) x y = PPP.sequence "; " x y
let field ?default name x = PPP.field ":" ", " ": " ?default name x

let union x = PPP.union "{" "}" ":" groupings delims string x
let (|||) x y = PPP.alternative " | " x y
let variant name x = PPP.variant ":" "" " of " name x

let tuple_open = "{"
let tuple_close = "}"
let tuple_sep = ","

let pair (p1 : 'a PPP.t) (p2 : 'b PPP.t) : ('a * 'b) PPP.t =
  record (
    field "fst" p1 <->
    field "snd" p2) >>:
  ((fun (v1, v2) -> Some v1, Some v2),
   (function Some v1, Some v2 -> v1, v2
           | _ -> assert false))

let triple (p1 : 'a PPP.t) (p2 : 'b PPP.t) (p3 : 'c PPP.t) : ('a * 'b * 'c) PPP.t =
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

include PPP.Ops
(* We must not open PPP in the tests or PPP bindings would overwrite again our
 * overwritten ones: *)
(*$inject
  let id x = x
  let of_string = PPP.of_string
  let to_string = PPP.to_string
 *)

let unit = cst "()"
(*$= unit
  (Some ((), 2)) (of_string unit "()" 0)
 *)

let char = char "'"

let float = PPP.float "nan" "inf"

let list (ppp : 'a PPP.t) : 'a list PPP.t =
  PPP.seq "list" "[" "]" ";" List.fold_left List.rev ppp
(*$= list & ~printer:id
   "[]" (to_string (list int) [])
   "[1;2;3]" (to_string (list int) [1;2;3])
 *)
(*$= list & ~printer:(function None -> "" | Some (l, i) -> Printf.sprintf "([%s], %d)" (List.fold_left (fun p i -> p ^(if p <> "" then ";" else "")^ string_of_int i) "" l) i)
  (Some ([], 2)) (of_string (list int) "[]" 0)
  (Some ([1;2;3], 7)) (of_string (list int) "[1;2;3]" 0)
 *)

let array (ppp : 'a PPP.t) : 'a array PPP.t =
  PPP.seq "array" "[|" "|]" ";" Array.fold_left (fun l -> Array.of_list (List.rev l)) ppp
(*$= array & ~printer:id
   "[||]" (to_string (array string) [||])
   "[|\"[\";\"|\";\";\"|]" (to_string (array string) [| "["; "|"; ";" |])
 *)
(*$= array & ~printer:(function None -> "" | Some (a, i) -> Printf.sprintf "([|%s|], %d)" (Array.fold_left (fun p i -> Printf.sprintf "%s%s%S" p (if p <> "" then ";" else "") i) "" a) i)
  (Some ([||], 4)) (of_string (array string) "[||]" 0)
  (Some ([| "1" ; "2" |], 11)) (of_string (array string) "[|\"1\";\"2\"|]" 0)
 *)

let tuple_open = "("
let tuple_close = ")"
let tuple_sep = ","
module Tuple = struct
  let first ppp = cst tuple_open -+ ppp
  let last ppp = ppp +- cst tuple_close
  let sep = cst tuple_sep

  let tuple2 (p1 : 'a PPP.t) (p2 : 'b PPP.t) : ('a * 'b) PPP.t =
    first p1 +- sep ++ last p2

  let tuple3 (p1 : 'a PPP.t) (p2 : 'b PPP.t) (p3 : 'c PPP.t) : ('a * 'b * 'c) PPP.t =
    first p1 +- sep ++ p2 +- sep ++ last p3 >>:
      ((fun (v1,v2,v3) -> (v1,v2),v3),
       (fun ((v1,v2),v3) -> v1,v2,v3))

  let tuple4 (p1 : 'a PPP.t) (p2 : 'b PPP.t) (p3 : 'c PPP.t) (p4 : 'd PPP.t) : ('a * 'b * 'c * 'd) PPP.t =
    first p1 +- sep ++ p2 +- sep ++ p3 +- sep ++ last p4 >>:
      ((fun (v1,v2,v3,v4) -> ((v1,v2),v3),v4),
       (fun (((v1,v2),v3),v4) -> v1,v2,v3,v4))

  let tuple5 (p1 : 'a PPP.t) (p2 : 'b PPP.t) (p3 :'c PPP.t) (p4 : 'd PPP.t) (p5 :'e PPP.t) : ('a * 'b * 'c * 'd * 'e) PPP.t =
    first p1 +- sep ++ p2 +- sep ++ p3 +- sep ++ p4 +- sep ++ last p5 >>:
      ((fun (v1,v2,v3,v4,v5) -> (((v1,v2),v3),v4),v5),
       (fun ((((v1,v2),v3),v4),v5) -> v1,v2,v3,v4,v5))
  (* you get the idea *)
end

let pair = Tuple.tuple2
(*$= pair & ~printer:id
   "(1,2)" (to_string (pair int int) (1,2))
   "(\"a\",2)" (to_string (pair string int) ("a", 2))
   "[|([],true)|]" (to_string (array (pair (list int) bool)) [|([],true)|])
 *)
(*$= pair & ~printer:(function None -> "" | Some ((v1,v2), i) -> Printf.sprintf "((%d,%S), %d)" v1 v2 i)
  (Some ((1,"a"), 7)) (of_string (pair int string) "(1,\"a\")" 0)
  (Some ((0,""), 6)) (of_string (pair int string) "(0,\"\")" 0)
  (Some ((0,""), 7)) (of_string (pair int string) "(0, \"\")" 0)
 *)

let triple = Tuple.tuple3
(*$= triple & ~printer:id
   "(1,2.1,\"a\")" (to_string (triple int float string) (1, 2.1, "a"))
   "(1,(1,2),3)" (to_string (triple int (pair int int) int) (1, (1,2), 3))
 *)
(*$= triple & ~printer:(function None -> "" | Some ((v1,v2,v3), i) -> Printf.sprintf "((%d,%S,%d), %d)" v1 v2 v3 i)
  (Some ((1,"a",1), 9)) (of_string (triple int string int) "(1,\"a\",1)" 0)
  (Some ((0,"",0), 8)) (of_string (triple int string int) "(0,\"\",0)" 0)
 *)

let groupings = [ "{","}" ; "[","]" ; "(",")" ; "[|","|]" ; "begin","end" ]
let delims = [ "," ; ";" ]

let record x = PPP.record "{" "}" "=" ";" groupings delims PPP.identifier x
let (<->) x y = PPP.sequence "; " x y
let field ?default name x = PPP.field "=" "; " ": " ?default name x

let union x = PPP.union "" "" "" groupings delims PPP.identifier x
let (|||) x y = PPP.alternative " | " x y
let variant name x = PPP.variant " " "" " of " name x

let result ok_ppp err_ppp = union (
  variant "Ok" ok_ppp |||
  variant "Err" err_ppp) >>:
  ((function Ok x -> Some x, None
           | Error e -> None, Some e),
   (function Some x, _-> Ok x
           | _, Some e -> Error e
           | _ -> assert false))
(*$inject
  let printer_of_ppp ppp = function
    | None -> "None"
    | Some (x, l) ->
      Printf.sprintf "Some(%s, %d)" (to_string ppp x) l
 *)
(*$= result & ~printer:(printer_of_ppp (result int string))
  (Some (Error "test", 10)) (of_string (result int string) "Err \"test\"" 0)
  (Some (Ok 42, 5)) (of_string (result int string) "Ok 42" 0)
 *)

let option ppp = union (
  variant "Some" ppp |||
  variant "None" none) >>:
  ((function Some x -> Some x, None
           | None -> None, Some ()),
   (function Some x, _ -> Some x
           | None, _ -> None))

(*$= option & ~printer:(printer_of_ppp (option int))
  (Some (Some 3, 6)) \
    (let ppp = option int in let s = Some 3 |> to_string ppp in of_string ppp s 0)
  (Some (None, 5)) \
    (let ppp = option int in let s = None   |> to_string ppp in of_string ppp s 0)
  (Some (Some 42, 7)) (of_string (option int) "Some 42" 0)
  (Some (Some 42, 9)) (of_string (option int) "Some (42)" 0)
 *)
(*$= option & ~printer:(printer_of_ppp (option (option int)))
  (Some (Some None, 9)) (of_string (option (option int)) "Some None" 0)
  (Some (Some None, 11)) (of_string (option (option int)) "Some (None)" 0)
  (Some (Some None, 10)) (of_string (option (option int)) "Some(None)" 0)
  (Some (Some (Some 42), 14)) (of_string (option (option int)) "Some (Some 42)" 0)
  (Some (Some (Some 42), 16)) (of_string (option (option int)) "Some (Some (42))" 0)
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
  Q.(array_of_size Gen.small_nat (pair (small_list int) bool)) (test_id (array (pair (list int) bool)))
 *)

  (* Test none *)
(*$inject
  type test_var = A | B
  let test_var_ppp = union (
    variant "A" none|||
    variant "B" none) >>:
  ((function A -> Some (), None
           | B -> None, Some ()),
   (function Some (), _ -> A
           | _, Some () -> B
           | _ -> assert false))
  type test_none = test_var list
  let test_none_ppp = list test_var_ppp
 *)
 (*$= test_none_ppp & ~printer:(printer_of_ppp test_none_ppp)
   (Some ([A], 3)) (of_string test_none_ppp "[A]" 0)
   (Some ([A; B], 6)) (of_string test_none_ppp "[A; B]" 0)
  *)

  (* Some non-regression tests *)
(*$inject
   type string_first = Bar of string * int
   let string_first_ppp = union (
     variant "Bar" (pair string int)) >>:
     ((function Bar (x,y) -> (x,y)),
      (fun (x,y) -> Bar (x,y)))
 *)
(*$= string_first_ppp & ~printer:(printer_of_ppp string_first_ppp)
   (Some (Bar ("bla", 42), 13)) (of_string string_first_ppp "Bar(\"bla\",42)" 0)
   (Some (Bar ("bla", 42), 14)) (of_string string_first_ppp "Bar (\"bla\",42)" 0)
   (Some (Bar ("bla", 42), 17)) (of_string string_first_ppp "Bar ( \"bla\" , 42)" 0)
 *)
(*$inject
   type test_rec = { a : int ; b : string_first option }
   let test_rec_ppp = PPP_OCaml.(record (
       field "a" int <->
       field "b" (option string_first_ppp)) >>:
       ((function | { a; b } -> Some a, Some b),
        (function | Some a, Some b -> { a ; b }
                  | _ -> assert false)))
 *)
(*$= test_rec_ppp & ~printer:(printer_of_ppp test_rec_ppp)
  (Some ({ a=42; b= Some (Bar("bla", 4)) }, 36)) \
    (of_string test_rec_ppp "{ a = 42 ; b = Some (Bar(\"bla\",4)) }" 0)
  (Some ({ a=42; b= Some (Bar("bla", 4)) }, 40)) \
    (of_string test_rec_ppp "{ a = 42 ; b = ((Some (Bar(\"bla\",4)))) }" 0)
 *)

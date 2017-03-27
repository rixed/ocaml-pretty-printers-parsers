open PPP
(*$inject open PPP *)
(*$inject let id x = x *)

(* Import the infix operators *)
let (++) = (++)
let (-+) = (-+)
let (+-) = (+-)
let (>>:) = (>>:)

let unit : unit t = cst "()"
(*$= unit
  (Some ((), 2)) (of_string unit "()" 0)
 *)

let bool = bool
let int = int
let float = float
let string = string

let list (ppp : 'a t) : 'a list t =
  seq "list" "[" "]" ";" List.iteri List.rev ppp
(*$= list & ~printer:id
   "[]" (to_string (list int) [])
   "[1;2;3]" (to_string (list int) [1;2;3])
 *)
(*$= list & ~printer:(function None -> "" | Some (l, i) -> Printf.sprintf "([%s], %d)" (List.fold_left (fun p i -> p ^(if p <> "" then ";" else "")^ string_of_int i) "" l) i)
  (Some ([], 2)) (of_string (list int) "[]" 0)
  (Some ([1;2;3], 7)) (of_string (list int) "[1;2;3]" 0)
 *)

let array (ppp : 'a t) : 'a array t =
  seq "array" "[|" "|]" ";" Array.iteri (fun l -> Array.of_list (List.rev l)) ppp
(*$= array & ~printer:id
   "[||]" (to_string (array string) [||])
   "[|\"[\";\"|\";\";\"|]" (to_string (array string) [| "["; "|"; ";" |])
 *)
(*$= array & ~printer:(function None -> "" | Some (a, i) -> Printf.sprintf "([|%s|], %d)" (Array.fold_left (fun p i -> Printf.sprintf "%s%s%S" p (if p <> "" then ";" else "") i) "" a) i)
  (Some ([||], 4)) (of_string (array string) "[||]" 0)
  (Some ([| "1" ; "2" |], 11)) (of_string (array string) "[|\"1\";\"2\"|]" 0)
 *)

module Tuple = struct
  let first ppp = cst "(" -+ ppp
  let last ppp = ppp +- cst ")"
  let sep = cst ","

  let tuple2 (p1 : 'a t) (p2 : 'b t) : ('a * 'b) t =
    first p1 +- sep ++ last p2

  let tuple3 (p1 : 'a t) (p2 : 'b t) (p3 : 'c t) : ('a * 'b * 'c) t =
    first p1 +- sep ++ p2 +- sep ++ last p3 >>:
      ((fun (v1,v2,v3) -> (v1,v2),v3),
       (fun ((v1,v2),v3) -> v1,v2,v3))

  let tuple4 (p1 : 'a t) (p2 : 'b t) (p3 : 'c t) (p4 : 'd t) : ('a * 'b * 'c * 'd) t =
    first p1 +- sep ++ p2 +- sep ++ p3 +- sep ++ last p4 >>:
      ((fun (v1,v2,v3,v4) -> ((v1,v2),v3),v4),
       (fun (((v1,v2),v3),v4) -> v1,v2,v3,v4))

  let tuple5 (p1 : 'a t) (p2 : 'b t) (p3 :'c t) (p4 : 'd t) (p5 :'e t) : ('a * 'b * 'c * 'd * 'e) t =
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

let record x = PPP.record "{" "}" "=" ";" groupings delims identifier x
let (<->) = PPP.(<->)
let field ?default name x = PPP.field "=" "; " ?default name x

let union x = PPP.union "" "" "" groupings delims identifier x
let (|||) = PPP.(|||)
let variant name x = PPP.variant "" "" " of " name x

let result ok_ppp err_ppp = union (
  variant "Ok" ok_ppp |||
  variant "Err" err_ppp) >>:
  ((function Ok x -> Some x, None
           | Error e -> None, Some e),
   (function Some x, _-> Ok x
           | _, Some e -> Error e
           | _ -> assert false))
(*$= result & ~printer:(function None -> "" | Some (Ok x, o) -> Printf.sprintf "Ok(%d,%d)" x o | Some (Error e, o) -> Printf.sprintf "Err(%s,%d)" e o)
  (Some (Error "test", 10)) (of_string (result int string) "Err \"test\"" 0)
  (Some (Ok 42, 5)) (of_string (result int string) "Ok 42" 0)
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
  Q.(pair printable_string (list int)) (test_id (pair string (list int)))
  Q.string (test_id string)
  Q.(array (pair (list int) bool)) (test_id (array (pair (list int) bool)))
 *)

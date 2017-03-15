let to_string (p,_) v =
  let buf = Buffer.create 100 in
  let o str = Buffer.add_string buf str in
  p v o ;
  Buffer.contents buf

let to_out_channel chan (p,_) v = p v (output_string chan)
let to_stdout pp v = to_out_channel stdout pp v
let to_stderr pp v = to_out_channel stderr pp v

let of_string (_,p) s =
  let i o l =
    if o + l > String.length s then
      String.sub s o (String.length s - o)
    else
      String.sub s o l in
  p i

let next_eq w i o =
  if i o (String.length w) = w then
    true, o + String.length w
  else
    false, o

let is_letter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'

let next_word_eq w i o =
  match next_eq w i o with
  | true, o ->
    let sep = i o 1 in
    (sep = "" ||
     let c = sep.[0] in
     not (is_letter c || is_digit c || c = '_')), o
  | x -> x

module OCaml=
struct
  (*$< OCaml *)
  (* [v] is the value, [o] is the output, [i] is the input *)
  let bool =
    (fun v o -> o (if v then "true" else "false")),
    (fun i o ->
      match next_word_eq "true" i o with
      | false, _ ->
        (match next_word_eq "false" i o with
        | true, o -> Some (false, o)
        | _ -> None)
      | x -> Some x)
(*$= bool & ~printer:(fun x -> x)
  "true" (to_string bool true)
  "false" (to_string bool false)
 *)
(*$= bool
  (Some (true, 4)) (of_string bool "true" 0)
  (Some (false, 5)) (of_string bool "false" 0)
*)

  let zero = Char.code '0'
  let str_is_digit s = String.length s > 0 && is_digit s.[0]
  let digit_of s = Char.code s.[0] - zero
  (* General format: [sign] digits *)
  type int_part = IntStart | Int
  let int =
    (fun v o -> o (string_of_int v)),
    (fun i o ->
      let rec loop o oo s n part =
        match part, i o 1 with
        | IntStart, "+" -> loop (o+1) oo s n Int
        | IntStart, "-" -> loop (o+1) oo (~- s) n Int
        | (IntStart|Int), d when str_is_digit d ->
          loop (o+1) (o+1) s (n*10 + digit_of d) Int
        | _ -> oo, s, n in
      let oo, s, n = loop o o 1 0 IntStart in
      if oo > o then Some (s*n, oo) else None)
(*$= int & ~printer:(fun x -> x)
  "42" (to_string int 42)
  "-42" (to_string int (-42))
  "0" (to_string int 0)
 *)
(*$= int & ~printer:(function None -> "" | Some (v,i) -> Printf.sprintf "(%d, %d)" v i)
  (Some (42, 2)) (of_string int "42" 0)
  (Some (-42, 3)) (of_string int "-42" 0)
  (Some (0, 1)) (of_string int "0" 0)
  (Some (5, 1)) (of_string int "5glop" 0)
  (Some (0, 2)) (of_string int "+0" 0)
  (Some (0, 2)) (of_string int "-0" 0)
  None (of_string int "-glop" 0)
  None (of_string int "+glop" 0)
 *)

  (* General format: [sign] digits ["." [FFF]] [e [sign] EEE] *)
  type float_part = IntStart | Int | Frac | ExpStart | Exp
  let float =
    (fun v o -> o (string_of_float v)),
    (fun i o ->
      let rec loop o oo s n sc es exp part =
        match part, i o 1 with
        | IntStart, "+" -> loop (o+1) oo s n sc es exp Int
        | IntStart, "-" -> loop (o+1) oo (~- s) n sc es exp Int
        | (IntStart|Int), d when str_is_digit d ->
          loop (o+1) (o+1) s (n * 10 + digit_of d) sc es exp Int
        | Int, "." -> loop (o+1) (o+1) s n sc es exp Frac
        | (Int|Frac), ("e"|"E") -> loop (o+1) oo s n sc es exp ExpStart
        | Frac, d when str_is_digit d ->
          loop (o+1) (o+1) s (n * 10 + digit_of d) (sc+1) es exp Frac
        | ExpStart, "+" -> loop (o+1) oo s n sc es exp Exp
        | ExpStart, "-" -> loop (o+1) oo s n sc (~- es) exp Exp
        | (ExpStart|Exp), d when str_is_digit d ->
          loop (o+1) (o+1) s n sc es (exp * 10 + digit_of d) Exp
        | _ -> oo, s, n, sc, es, exp in
      let oo, s, n, sc, es, exp = loop o o 1 0 0 1 0 IntStart in
      if oo > o then Some (
        float_of_int (s * n) *. 10. ** float_of_int (es * exp - sc), oo)
      else None)
(*$= float & ~printer:(fun x -> x)
  "-0.00010348413604" (to_string float (-0.00010348413604))
 *)
(*$= float & ~printer:(function None -> "" | Some (f,i) -> Printf.sprintf "(%f, %d)" f i)
  (Some (3.14, 4)) (of_string float "3.14" 0)
  (Some (3.14, 6)) (of_string float "314e-2" 0)
  (Some (3.14, 8)) (of_string float "0.0314E2" 0)
  (Some (~-.3.14, 9)) (of_string float "-0.0314E2" 0)
  (Some (42., 2)) (of_string float "42" 0)
  (Some (~-.42., 3)) (of_string float "-42" 0)
  (Some (42., 3)) (of_string float "42." 0)
  (Some (~-.42., 4)) (of_string float "-42." 0)
  (Some (42., 5)) (of_string float "+42e0" 0)
  (Some (42., 6)) (of_string float "+42.e0" 0)
  (Some (42., 7)) (of_string float "+42.0e0" 0)
  None (of_string float "glop" 0)
  None (of_string float "+glop" 0)
  None (of_string float "-glop" 0)
  (Some (1., 1)) (of_string float "1e" 0)
  (Some (-0.00010348413604, 17)) (of_string float "-0.00010348413604" 0)
 *)
 
  type string_part = First | Char | BackslashStart | Backslash
  let string_of lst len =
    let s = Bytes.create len in
    let rec loop i lst =
      if i >= 0 then (
        Bytes.set s i (List.hd lst) ;
        loop (i-1) (List.tl lst)
      ) in
    loop (len-1) lst ;
    Bytes.to_string s
  (*$= string_of & ~printer:(fun x -> x)
    "glop" (string_of ['p';'o';'l';'g'] 4)
    "" (string_of [] 0)
   *)

  (* Format: "..." *)
  let string =
    (fun v o -> o (Printf.sprintf "%S" v)),
    (fun i o ->
      let rec loop o l s bsn part =
        match part, i o 1 with
        | First, "\"" -> loop (o+1) l s bsn Char
        | Char, "\\" -> loop (o+1) l s bsn BackslashStart
        | Char, "\"" -> (* The only successful termination *)
          Some (string_of l s, o+1)
        | Char, d when String.length d > 0 ->
          loop (o+1) (d.[0]::l) (s+1) bsn Char
        | BackslashStart, "\\" -> loop (o+1) ('\\'::l) (s+1) bsn Char
        | BackslashStart, "\"" -> loop (o+1) ('"'::l) (s+1) bsn Char
        | BackslashStart, "\'" -> loop (o+1) ('\''::l) (s+1) bsn Char
        | BackslashStart, "n" -> loop (o+1) ('\n'::l) (s+1) bsn Char
        | BackslashStart, "r" -> loop (o+1) ('\r'::l) (s+1) bsn Char
        | BackslashStart, "t" -> loop (o+1) ('\t'::l) (s+1) bsn Char
        | BackslashStart, "b" -> loop (o+1) ('\b'::l) (s+1) bsn Char
        | BackslashStart, d when str_is_digit d ->
          (* 10+ so that we know when we have had 3 digits: *)
          loop (o+1) l s (10 + digit_of d) Backslash
        | Backslash, d when str_is_digit d ->
          if bsn >= 100 then ( (* we already had 2 digits *)
            let bsn = (bsn - 100) * 10 + digit_of d in
            if bsn > 255 then None
            else loop (o+1) (Char.chr bsn :: l) (s+1) 0 Char
          ) else (
            (* 10+ so that we know when we have had 3 digits: *)
            loop (o+1) l s (10*bsn + digit_of d) Backslash
          )
        | _ -> None (* everything else is game-over *) in
      loop o [] 0 0 First)
  (*$= string & ~printer:(fun x -> x)
     "\"glop\"" (to_string string "glop")
     "\"\"" (to_string string "")
     "\"\\207\"" (to_string string "\207")
   *)
  (*$= string & ~printer:(function None -> "" | Some (s, i) -> Printf.sprintf "(%s, %d)" s i)
    (Some ("glop", 6)) (of_string string "\"glop\"" 0)
    (Some ("gl\bop\n", 10)) (of_string string "\"gl\\bop\\n\"" 0)
    (Some ("\207", 6)) (of_string string "\"\\207\"" 0)
   *)

  let seq opn cls sep iteri of_rev_list (p, s) =
    (fun v o ->
      o opn ;
      iteri (fun i v' ->
        if i > 0 then o sep ;
        p v' o) v ;
      o cls),
    (fun i o ->
      let rec parse_sep prev o =
        if i o (String.length cls) = cls then
          Some (of_rev_list prev, o + String.length cls)
        else if i o (String.length sep) = sep then
          parse_item prev (o + String.length sep)
        else
          None
      and parse_item prev o =
        match s i o with
        | None -> None
        | Some (x, o') -> parse_sep (x::prev) o' in
      if i o (String.length opn) = opn then (
        if i (o + String.length opn) (String.length cls) = cls then
          Some (of_rev_list [], o + String.length opn + String.length cls)
        else
          parse_item [] (o + String.length opn)
      ) else None)

  let list item = seq "[" "]" ";" List.iteri List.rev item
  (*$= list & ~printer:(fun x -> x)
     "[]" (to_string (list int) [])
     "[1;2;3]" (to_string (list int) [1;2;3])
   *)
  (*$= list & ~printer:(function None -> "" | Some (l, i) -> Printf.sprintf "([%s], %d)" (List.fold_left (fun p i -> p ^(if p <> "" then ";" else "")^ string_of_int i) "" l) i)
    (Some ([], 2)) (of_string (list int) "[]" 0)
    (Some ([1;2;3], 7)) (of_string (list int) "[1;2;3]" 0)
   *)

  let array item = seq "[|" "|]" ";" Array.iteri (fun l -> Array.of_list (List.rev l)) item
  (*$= array & ~printer:(fun x -> x)
     "[||]" (to_string (array string) [||])
     "[|\"[\";\"|\";\";\"|]" (to_string (array string) [| "["; "|"; ";" |])
   *)
  (*$= array & ~printer:(function None -> "" | Some (a, i) -> Printf.sprintf "([|%s|], %d)" (Array.fold_left (fun p i -> Printf.sprintf "%s%s%S" p (if p <> "" then ";" else "") i) "" a) i)
    (Some ([||], 4)) (of_string (array string) "[||]" 0)
    (Some ([| "1" ; "2" |], 11)) (of_string (array string) "[|\"1\";\"2\"|]" 0)
   *)

  let (++) (p1, s1) (p2, s2) =
    (fun (v1, v2) o ->
      p1 v1 o ;
      p2 v2 o),
    (fun i o ->
      match s1 i o with
      | None -> None
      | Some (v1, o) ->
        (match s2 i o with
        | None -> None
        | Some (v2, o) -> Some ((v1, v2), o)))

  let (-+) (p1, s1) (p2, s2) =
    (fun v2 o ->
      p1 () o ;
      p2 v2 o),
    (fun i o ->
      match s1 i o with
      | None -> None
      | Some ((), o) -> s2 i o)

  let (+-) (p1, s1) (p2, s2) =
    (fun v1 o ->
      p1 v1 o ;
      p2 () o),
    (fun i o ->
      match s1 i o with
      | None -> None
      | Some (v, o) ->
        (match s2 i o with
        | None -> None
        | Some (_, o) -> Some (v, o)))

  let cst s =
    (fun () (o: string->unit) -> o s),
    (fun i o ->
      let l = String.length s in
      if i o l = s then Some ((), o+l) else None)

  let (>>:) (p, s) (f,f') =
    (fun v o -> p (f v) o),
    (fun i o ->
      match s i o with
      | None -> None
      | Some (x, o) -> Some (f' x, o))

  module Tuple = struct
    let first p = cst "(" -+ p
    let last p = p +- cst ")"
    let sep = cst ","

    let tuple2 p1 p2 =
      first p1 +- sep ++ last p2
    let tuple3 p1 p2 p3 =
      first p1 +- sep ++ p2 +- sep ++ last p3 >>:
        ((fun (v1,v2,v3) -> (v1,v2),v3),
         (fun ((v1,v2),v3) -> v1,v2,v3))
    let tuple4 p1 p2 p3 p4 =
      first p1 +- sep ++ p2 +- sep ++ p3 +- sep ++ last p4 >>:
        ((fun (v1,v2,v3,v4) -> ((v1,v2),v3),v4),
         (fun (((v1,v2),v3),v4) -> v1,v2,v3,v4))
    let tuple5 p1 p2 p3 p4 p5 =
      first p1 +- sep ++ p2 +- sep ++ p3 +- sep ++ p4 +- sep ++ last p5 >>:
        ((fun (v1,v2,v3,v4,v5) -> (((v1,v2),v3),v4),v5),
         (fun ((((v1,v2),v3),v4),v5) -> v1,v2,v3,v4,v5))
    (* you get the idea *)
  end

  let pair = Tuple.tuple2
  (*$= pair & ~printer:(fun x -> x)
     "(1,2)" (to_string (pair int int) (1,2))
     "(\"a\",2)" (to_string (pair string int) ("a", 2))
     "[|([],true)|]" (to_string (array (pair (list int) bool)) [|([],true)|])
   *)
  (*$= pair & ~printer:(function None -> "" | Some ((v1,v2), i) -> Printf.sprintf "((%d,%S), %d)" v1 v2 i)
    (Some ((1,"a"), 7)) (of_string (pair int string) "(1,\"a\")" 0)
    (Some ((0,""), 6)) (of_string (pair int string) "(0,\"\")" 0)
   *)

  let triple = Tuple.tuple3
  (*$= triple & ~printer:(fun x -> x)
     "(1,2.1,\"a\")" (to_string (triple int float string) (1, 2.1, "a"))
     "(1,(1,2),3)" (to_string (triple int (pair int int) int) (1, (1,2), 3))
   *)
  (*$= triple & ~printer:(function None -> "" | Some ((v1,v2,v3), i) -> Printf.sprintf "((%d,%S,%d), %d)" v1 v2 v3 i)
    (Some ((1,"a",1), 9)) (of_string (triple int string int) "(1,\"a\",1)" 0)
    (Some ((0,"",0), 8)) (of_string (triple int string int) "(0,\"\",0)" 0)
   *)

  (* Records: We can't do it in general but we can help build specific ones.
   * Notes: Fields order matters as we return a tuple. *)
  module Record = struct
    let field ?(last=false) name (p, s) =
      (fun v o ->
        o (name ^"=") ;
        p v o ;
        o (if last then "}" else ";")),
      (fun i o ->
        match next_word_eq name i o with
        | false, _ -> None
        | true, o ->
          if i o 1 <> "=" then None else
          (match s i (o+1) with
          | None -> None
          | Some (v, o) ->
            if i o 1 <> (if last then "}" else ";") then None else
            Some (v, o+1)))
            
    let first name p = cst "{" -+ field name p
    let last name p = field ~last:true name p
  end

  (*$inject
    type person = { name: string ; age: int ; male : bool }
    let person =
      Record.((first "name" string) ++ (field "age" int) ++ (last "male" bool)) >>:
        ((fun { name ; age ; male } -> ((name, age), male)),
         (fun ((name, age), male) -> { name ; age ; male }))
   *)
  (*$= person & ~printer:(fun x -> x)
    "{name=\"John\";age=41;male=true}" (to_string person { name = "John"; age = 41; male = true })
   *)
  (*$= person & ~printer:(function None -> "" | Some (p, o) -> Printf.sprintf "(%s, %d)" (to_string person p) o)
    (Some ({ name = "John"; age = 41; male = true }, 30)) (of_string person "{name=\"John\";age=41;male=true}" 0)
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

  (*$>*)
end

(* Useful for conditional printing: *)

let delayed (p, s) =
  (fun v o -> p (v ()) o),
  (fun i o -> s (i ()) o)
(*$= delayed
  "42" (to_string (delayed OCaml.int) (fun () -> 42))
 *)


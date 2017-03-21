type writer = string -> unit
type reader = int -> int -> string
type 'a t = (writer -> 'a -> unit) *
            (reader -> int -> ('a * int) option)

let may f = function None -> () | Some x -> f x
let map f = function None -> None | Some x -> Some (f x)

let to_string (p,_) v =
  let buf = Buffer.create 100 in
  let o str = Buffer.add_string buf str in
  p o v ;
  Buffer.contents buf

let to_out_channel chan (p,_) v = p (output_string chan) v
let to_stdout pp v = to_out_channel stdout pp v
let to_stderr pp v = to_out_channel stderr pp v

let string_reader s o l =
  if o + l > String.length s then
    String.sub s o (String.length s - o)
  else
    String.sub s o l

let of_string (_,p) s =
  p (string_reader s)

let next_eq w i o =
  if i o (String.length w) = w then
    true, o + String.length w
  else
    false, o

let is_letter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let zero = Char.code '0'
let str_is_digit s = String.length s > 0 && is_digit s.[0]
let digit_of s = Char.code s.[0] - zero

let rec chop_sub s b e =
  if b >= e then "" else
  if is_blank s.[b] then chop_sub s (b+1) e else
  if is_blank s.[e-1] then chop_sub s b (e-1) else
  String.sub s b (e-b)
(*$inject let id x = x *)
(*$= chop_sub & ~printer:id
  "glop" (chop_sub "glop" 0 4)
  "glop" (chop_sub " glop" 0 5)
  "glop" (chop_sub "glop " 0 5)
  "glop" (chop_sub "glop	" 0 5)
  "glop" (chop_sub "  glop" 0 6)
  "glop" (chop_sub "	 	glop " 0 8)
  "" (chop_sub "" 0 0)
  "" (chop_sub " " 0 1)
  "" (chop_sub " " 1 1 )
*)

let rec skip_blanks i o =
  match i o 1 with
  | " " -> skip_blanks i (o + 1)
  | _ -> o

let rec skip_word i o =
  let s = i o 1 in
  if s = "" then o else (
    let c = s.[0] in
    if is_blank c || c = ',' || c = ';' || c = '(' || c = '{' || c = '[' then
      o
    else skip_word i (o+1))

let next_word i o =
  let o' = skip_word i o in
  if o' = o then None else
  Some (i o (o'-o), o')

(* Start with a letter of underscore, then can contain digits. *)
let identifier =
  (fun o x -> o x),
  (fun i o ->
    let rec loop oo =
      let s = i oo 1 in
      if s = "" then oo else
      let c = s.[0] in
      if is_letter c || c = '_' || oo > o && is_digit c then
        loop (oo+1)
      else
        oo in
    let oo = loop o in
    if oo > o then Some (i o oo, oo) else None)
(*$= identifier & ~printer:(function None -> "" | Some (i, o) -> Printf.sprintf "(%s,%d)" i o)
  (Some ("glop", 4)) (of_string identifier "glop" 0)
  (Some ("glop", 4)) (of_string identifier "glop\n" 0)
  (Some ("glop0", 5)) (of_string identifier "glop0" 0)
  None (of_string identifier "0glop" 0)
 *)

let next_word_eq w i o =
  match next_eq w i o with
  | true, o ->
    let sep = i o 1 in
    (sep = "" ||
     let c = sep.[0] in
     not (is_letter c || is_digit c || c = '_')), o
  | x -> x

let next_int i o =
  let rec loop n o =
    let s = i o 1 in
    if str_is_digit s then (
      loop (n*10 + digit_of s) (o+1)
    ) else (
      n, o
    ) in
  let n, o' = loop 0 o in
  if o' > o then Some (n, o') else None
(*$= next_int & ~printer:(function None -> "" | Some (n, o) -> Printf.sprintf "(%d, %d)" n o)
  (Some (42, 2)) (next_int (string_reader "42glop") 0)
 *)

let rec until u i o =
  let l = String.length u in
  let s = i o l in
  if String.length s < l then None else
  if s <> u then until u i (o + 1) else
  Some (o + l)

let string_of lst len =
  let s = Bytes.create len in
  let rec loop i lst =
    if i >= 0 then (
      Bytes.set s i (List.hd lst) ;
      loop (i-1) (List.tl lst)
    ) in
  loop (len-1) lst ;
  Bytes.to_string s
(*$= string_of & ~printer:id
  "glop" (string_of ['p';'o';'l';'g'] 4)
  "" (string_of [] 0)
 *)

let seq opn cls sep iteri of_rev_list (p, s) =
  (fun o v ->
    o opn ;
    iteri (fun i v' ->
      if i > 0 then o sep ;
      p o v') v ;
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

let (++) (p1, s1) (p2, s2) =
  (fun o (v1, v2) ->
    p1 o v1 ;
    p2 o v2),
  (fun i o ->
    match s1 i o with
    | None -> None
    | Some (v1, o) ->
      s2 i o |> map (fun (v2, o) -> (v1, v2), o))

let (-+) (p1, s1) (p2, s2) =
  (fun o v2 ->
    p1 o () ;
    p2 o v2),
  (fun i o ->
    match s1 i o with
    | None -> None
    | Some ((), o) -> s2 i o)

let (+-) (p1, s1) (p2, s2) =
  (fun o v1 ->
    p1 o v1 ;
    p2 o ()),
  (fun i o ->
    match s1 i o with
    | None -> None
    | Some (v, o) ->
      s2 i o |> map (fun (_, o) -> v, o))

(* Always allow blanks around the constant *)
let cst s =
  (fun (o: string->unit) () -> o s),
  (fun i o ->
    let o = skip_blanks i o in
    let l = String.length s in
    if i o l = s then (
      let o = skip_blanks i (o + l) in
      Some ((), o)
    ) else None)

let default v (p, s) =
  p,
  (fun i o ->
    match s i o with
    | None -> Some (v, o)
    | x -> x)
(*$= default & ~printer:(function None -> "" | Some (d, o) -> Printf.sprintf "(%d, %d)" d o)
  (Some (42,4)) (of_string (default 17 (cst "{" -+ OCaml.int +- cst "}")) "{42}" 0)
  (Some (17,0)) (of_string (default 17 (cst "{" -+ OCaml.int +- cst "}")) "pas glop" 0)
 *)

let optional (p, s) =
  (fun o -> function None -> () | Some x -> p o x),
  (fun i o ->
    match s i o with
    | Some (x, o') -> Some (Some x, o')
    | None -> Some (None, o))
(*$= optional & ~printer:(function None -> "" | Some (None, _) -> Printf.sprintf "none" | Some (Some d, o) -> Printf.sprintf "(%d, %d)" d o)
  (Some (Some 42,4)) (of_string (optional (cst "{" -+ OCaml.int +- cst "}")) "{42}" 0)
  (Some (None,0)) (of_string (optional (cst "{" -+ OCaml.int +- cst "}")) "pas glop" 0)
 *)

let (>>:) (p, s) (f,f') =
  (fun o v -> p o (f v)),
  (fun i o ->
    s i o |> map (fun (x,o) -> f' x, o))


module OCaml=
struct
  (*$< OCaml *)

  let unit : unit t =
    (fun o () -> o "()"),
    (fun i o ->
      let o = skip_blanks i o in
      if i o 2 = "()" then Some ((), o+2) else None)
  (*$= unit
    (Some ((), 2)) (of_string unit "()" 0)
   *)

  let bool : bool t =
    (fun o v -> o (if v then "true" else "false")),
    (fun i o ->
      match next_word_eq "true" i o with
      | false, _ ->
        (match next_word_eq "false" i o with
        | true, o -> Some (false, o)
        | _ -> None)
      | x -> Some x)
(*$= bool & ~printer:id
  "true" (to_string bool true)
  "false" (to_string bool false)
 *)
(*$= bool
  (Some (true, 4)) (of_string bool "true" 0)
  (Some (false, 5)) (of_string bool "false" 0)
*)

  (* General format: [sign] digits *)
  type int_part = IntStart | Int
  let int : int t =
    (fun o v -> o (string_of_int v)),
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
(*$= int & ~printer:id
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
  let float : float t =
    (fun o v -> o (string_of_float v)),
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
(*$= float & ~printer:id
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

  (* Format: "..." *)
  type string_part = First | Char | BackslashStart | Backslash
  let string : string t =
    (fun o v -> o (Printf.sprintf "%S" v)),
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
  (*$= string & ~printer:id
     "\"glop\"" (to_string string "glop")
     "\"\"" (to_string string "")
     "\"\\207\"" (to_string string "\207")
   *)
  (*$= string & ~printer:(function None -> "" | Some (s, i) -> Printf.sprintf "(%s, %d)" s i)
    (Some ("glop", 6)) (of_string string "\"glop\"" 0)
    (Some ("gl\bop\n", 10)) (of_string string "\"gl\\bop\\n\"" 0)
    (Some ("\207", 6)) (of_string string "\"\\207\"" 0)
   *)

  let list (p : 'a t) : 'a list t =
    seq "[" "]" ";" List.iteri List.rev p
  (*$= list & ~printer:id
     "[]" (to_string (list int) [])
     "[1;2;3]" (to_string (list int) [1;2;3])
   *)
  (*$= list & ~printer:(function None -> "" | Some (l, i) -> Printf.sprintf "([%s], %d)" (List.fold_left (fun p i -> p ^(if p <> "" then ";" else "")^ string_of_int i) "" l) i)
    (Some ([], 2)) (of_string (list int) "[]" 0)
    (Some ([1;2;3], 7)) (of_string (list int) "[1;2;3]" 0)
   *)

  let array (p : 'a t) : 'a array t =
    seq "[|" "|]" ";" Array.iteri (fun l -> Array.of_list (List.rev l)) p
  (*$= array & ~printer:id
     "[||]" (to_string (array string) [||])
     "[|\"[\";\"|\";\";\"|]" (to_string (array string) [| "["; "|"; ";" |])
   *)
  (*$= array & ~printer:(function None -> "" | Some (a, i) -> Printf.sprintf "([|%s|], %d)" (Array.fold_left (fun p i -> Printf.sprintf "%s%s%S" p (if p <> "" then ";" else "") i) "" a) i)
    (Some ([||], 4)) (of_string (array string) "[||]" 0)
    (Some ([| "1" ; "2" |], 11)) (of_string (array string) "[|\"1\";\"2\"|]" 0)
   *)

  module Tuple = struct
    let first p = cst "(" -+ p
    let last p = p +- cst ")"
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

  (* Skip until end of string. Used by skip_any. *)
  let rec skip_string ?(backslashed=false) i o =
    match i o 1 with
    | "\"" -> if backslashed then skip_string i (o+1) else Some (o+1)
    | "\\" -> skip_string ~backslashed:(not backslashed) i (o + 1)
    | "" -> None
    | _ -> skip_string i (o+1)

  let rec skip_delim ?(had_delim=false) i o =
    match i o 1 with
    | ("," | ";") when not had_delim -> skip_delim ~had_delim:true i (o+1)
    | s when String.length s > 0 && is_blank s.[0] -> skip_delim i (o+1)
    | _ -> if had_delim then Some o else None

  (* To be able to "reorder" records fields we need a function able to tell
   * us the length of a value, without knowing its type. *)
  (* format: blanks* opening_char (any value list)* closing_char | everything_but_blanks_or_openeing_or_closing_chars *)
  let rec skip_any i o =
    let str = i o 2 in
    if str = "[|" then skip_group "|]" i (o+2)
    else if str = "" then Some o else (
      let c = str.[0] in
      if c = '(' then skip_group ")" i (o+1)
      else if c = '[' then skip_group "]" i (o+1)
      else if c = '{' then skip_group "}" i (o+1)
      else if c = ')' || c = ']' || c = '}' || c = '|' || c = ';' || c = ',' then Some o
      else if c = '"' then skip_string i (o+1)
      else skip_any i (o+1)) (* anything else including blanks is "the value" that we skip *)
  and skip_group ?(f=(fun _ _ _ -> ())) cls i o =
    let clsl = String.length cls in
    match skip_any i o with
    | None ->
      (* Maybe we are done? *)
      if i o clsl = cls then Some (o + clsl) else None
    | Some o' ->
      let o'' = skip_blanks i o' in
      let s = i o'' clsl in
      if s = cls then (
        f i o (o'-o) ;
        Some (o'' + clsl)
      ) else if s = "" then None
      else if s.[0] = ',' || s.[0] = ';' then (
        f i o (o'-o) ;
        skip_group ~f cls i (o'' + 1)
      ) else None
  (*$= skip_any & ~printer:(function None -> "" | Some o -> string_of_int o)
    (Some 4) (skip_any (string_reader "glop") 0)
    (Some 8) (skip_any (string_reader "pas glop]") 0)
    (Some 6) (skip_any (string_reader "(glop)") 0)
    (Some 8) (skip_any (string_reader "[|glop|]") 0)
    (Some 2) (skip_any (string_reader "()") 0)
    (Some 7) (skip_any (string_reader "[1;2;3]") 0)
    (Some 13) (skip_any (string_reader "[ 1 ; 2 ; 3 ]") 0)
    (Some 10) (skip_any (string_reader " \"bl\\\"a][\" z") 0)

    (Some 26) (skip_any (string_reader "{ a = 43 ; glop=(1, 2 ); } ") 0)
    None (skip_any (string_reader "{ a = 43 ; glop=1, 2 ); } ") 0)
   *)

  let record (p,s) =
    (fun (o : string->unit) v ->
      o "{" ;
      p o v ;
      o "}"),
    (fun i o ->
      let h = Hashtbl.create 11 in
      let valid = ref true in
      let f i o l =
        let v = i o l in
        match String.index v '=' with
        | exception Not_found ->
          valid := false
        | idx ->
          if idx = 0 || idx >= l-1 then (
            valid := false
          ) else (
            let name = chop_sub v 0 idx
            and value = chop_sub v (idx+1) l in
            Hashtbl.replace h name value
          ) in
      let o = skip_blanks i o in
      match i o 1 with
      | "{" ->
        let o = skip_blanks i (o+1) in
        (match skip_group ~f "}" i o with
        | None -> None
        | Some o ->
          if !valid then (
            s h |> map (fun x -> x, o)
          ) else None)
      | _ -> None)

  let field ?default name (p, s) =
    (fun o v ->
      o (name ^"=") ;
      p o v),
    (fun h ->
      match Hashtbl.find h name with
      | exception Not_found -> default
      | str ->
        s (string_reader str) 0 |> map fst)

  (* Compose 2 fields *)
  let (<->) (p1,s1) (p2,s2) =
    (fun o (v1, v2) ->
      p1 o v1 ;
			o ";" ;
      p2 o v2),
    (fun h ->
      match s1 h with
      | None -> None
      | Some v1 ->
        s2 h |> map (fun v2 -> v1, v2))

  (*$inject
    type person = { name: string ; age: int ; male : bool }

    let person : person t =
      record (field "name" string <-> field "age" int <->
              field "male" ~default:true bool) >>:
        ((fun { name ; age ; male } -> (name, age), male),
         (fun ((name, age), male) -> { name ; age ; male }))
   *)
  (*$= person & ~printer:id
    "{name=\"John\";age=41;male=true}" (to_string person { name = "John"; age = 41; male = true })
   *)
  (*$= person & ~printer:(function None -> "" | Some (p, o) -> Printf.sprintf "(%s, %d)" (to_string person p) o)
    (Some ({ name = "John"; age = 41; male = true }, 30)) \
      (of_string person "{name=\"John\";age=41;male=true}" 0)
    (Some ({ name = "John"; age = 41; male = true }, 30)) \
      (of_string person "{age=41;name=\"John\";male=true}" 0)
    (Some ({ name = "John"; age = 41; male = true }, 39)) \
      (of_string person " {  age=41 ; name = \"John\" ;male =true}" 0)
    (Some ({name="John";age=41;male=true}, 28)) \
      (of_string person " { age = 41 ; name = \"John\"}" 0)
   *)

  let union (p, s) =
    p,
    (fun i o ->
      let o = skip_blanks i o in
      match next_word i o with
      | None -> None
      | Some (name, o') ->
        (match skip_any i o' with
        | None -> None
        | Some o ->
          let value = chop_sub (i o' (o-o')) 0 (o-o') in
          s name value |> map (fun x -> x, o)))

  let (|||) (p1,s1) (p2,s2) =
    (fun (o : string -> unit) (v1,v2) ->
      may (p1 o) v1 ;
      may (p2 o) v2),
    (fun name value ->
      match s1 name value with
      | Some _ as x -> Some (x, None)
      | None ->
        (match s2 name value with
        | Some _ as x -> Some (None, x)
        | None -> None))

  (* turn a pretty-printer-parser into the version above usable by union: *)
  let variant name (p,s) =
    (fun o v -> o name ; p o v),
    (fun n v ->
      if n = name then (
        let i = string_reader v in
        s i 0 |> map fst
      ) else None)

  (* Like unit but with no representation, useful for
   * constructor without arguments *)
  let none : unit t =
    (fun _o () -> ()),
    (fun _i o -> Some ((), o))
  (*$= none
    (Some ((), 0)) (of_string none "" 0)
   *)

  (*$inject
    type color = RGB of (int * int * int)
               | Named of string
               | Transparent

    let color : color t = union (
      variant "RGB" (triple int int int) ||| variant "Named" string ||| variant "Transp" none) >>:
      ((function RGB rgb -> Some (Some rgb, None), None
               | Named name -> Some (None, Some name), None
               | Transparent -> None, Some ()),
       (function Some (Some rgb, _), _ -> RGB rgb
               | Some (_, Some name), _ -> Named name
               | _ -> Transparent))
   *)
  (*$= color & ~printer:id
    "RGB(0,0,255)" (to_string color (RGB (0, 0, 255)))
    "Transp" (to_string color Transparent)
   *)
  (*$= color & ~printer:(function None -> "" | Some (p, o) -> Printf.sprintf "(%s, %d)" (to_string color p) o)
    (Some (RGB(0,0,255), 12)) (of_string color "RGB(0,0,255)" 0)
    (Some (RGB(0,0,255), 13)) (of_string color "RGB (0,0,255)" 0)
    (Some (RGB(0,0,255), 15)) (of_string color "  RGB (0,0,255)" 0)
    (Some (RGB(0,0,255), 21)) (of_string color "RGB  ( 0 ,  0, 255  ) " 0)
    (Some (Transparent, 8)) (of_string color " Transp " 0)
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

let delayed ((p, s) : 'a t) : (unit -> 'a) t =
  (fun o v -> p o (v ())),
  (fun i o -> s i o |> map (fun (x, o) -> (fun () -> x), o))
(*$= delayed
  "42" (to_string (delayed OCaml.int) (fun () -> 42))
 *)


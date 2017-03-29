type writer = string -> unit
type reader = int -> int -> string
type 'a t = { printer : writer -> 'a -> unit ;
              scanner : reader -> int -> ('a * int) option ;
              descr : string }

let may f = function None -> () | Some x -> f x
let map f = function None -> None | Some x -> Some (f x)

let to_string ppp v =
  let buf = Buffer.create 100 in
  let o str = Buffer.add_string buf str in
  ppp.printer o v ;
  Buffer.contents buf

let to_out_channel chan ppp v = ppp.printer (output_string chan) v
let to_stdout pp v = to_out_channel stdout pp v
let to_stderr pp v = to_out_channel stderr pp v

let string_reader s o l =
  if o + l > String.length s then
    String.sub s o (String.length s - o)
  else
    String.sub s o l

let of_string ppp s = ppp.scanner (string_reader s)

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
  { printer = (fun o x -> o x) ;
    scanner = (fun i o ->
      let rec loop oo =
        let s = i oo 1 in
        if s = "" then oo else
        let c = s.[0] in
        if is_letter c || c = '_' || oo > o && is_digit c then
          loop (oo+1)
        else
          oo in
      let oo = loop o in
      if oo > o then Some (i o (oo-o), oo) else None) ;
    descr = "identifier" }
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
  Some o

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

let seq name opn cls sep iteri of_rev_list ppp =
  { printer = (fun o v ->
      o opn ;
      iteri (fun i v' ->
        if i > 0 then o sep ;
        ppp.printer o v') v ;
      o cls) ;
    scanner = (fun i o ->
      let rec parse_sep prev o =
        if i o (String.length sep) = sep then
          parse_item prev (o + String.length sep)
        else if i o (String.length cls) = cls then
          Some (of_rev_list prev, o + String.length cls)
        else
          None
      and parse_item prev o =
        match ppp.scanner i o with
        | Some (x, o') -> parse_sep (x::prev) o'
        | None ->
          if i o (String.length cls) = cls then
            Some (of_rev_list prev, o + String.length cls)
          else None in
      if i o (String.length opn) = opn then
        parse_item [] (o + String.length opn)
      else None) ;
    descr = name ^" of "^ ppp.descr }
(*$= seq & ~printer:(function None -> "" | Some (l,o) -> Printf.sprintf "(%s, %d)" (String.concat ";" l) o)
  (Some (["a";"b";"cde"], 9)) \
    (of_string (seq "list" "[" "]" ";" List.iteri List.rev identifier) "[a;b;cde]" 0)
  (Some (["a";"b";"cde"], 9)) \
    (of_string (seq "sequence" "" "" "--" List.iteri List.rev identifier) "a--b--cde" 0)
 *)

let (++) ppp1 ppp2 =
  { printer = (fun o (v1, v2) ->
      ppp1.printer o v1 ;
      ppp2.printer o v2) ;
    scanner = (fun i o ->
      match ppp1.scanner i o with
      | None -> None
      | Some (v1, o) ->
        ppp2.scanner i o |> map (fun (v2, o) -> (v1, v2), o)) ;
    descr = ppp1.descr ^", "^ ppp2.descr }

let (-+) ppp1 ppp2 =
  { printer = (fun o v2 ->
      ppp1.printer o () ;
      ppp2.printer o v2) ;
    scanner = (fun i o ->
      match ppp1.scanner i o with
      | None -> None
      | Some ((), o) -> ppp2.scanner i o) ;
    descr = ppp2.descr }

let (+-) ppp1 ppp2 =
  { printer = (fun o v1 ->
      ppp1.printer o v1 ;
      ppp2.printer o ()) ;
    scanner = (fun i o ->
      match ppp1.scanner i o with
      | None -> None
      | Some (v, o) ->
        ppp2.scanner i o |> map (fun (_, o) -> v, o)) ;
    descr = ppp1.descr }

let (>>:) ppp (f,f') =
  { printer = (fun o v -> ppp.printer o (f v)) ;
    scanner = (fun i o ->
      ppp.scanner i o |> map (fun (x,o) -> f' x, o)) ;
    descr = ppp.descr }

(* Always allow blanks around the constant *)
let cst s =
  { printer = (fun (o: string->unit) () -> o s) ;
    scanner = (fun i o ->
      let o = skip_blanks i o in
      let l = String.length s in
      if i o l = s then (
        let o = skip_blanks i (o + l) in
        Some ((), o)
      ) else None) ;
    descr = "" }

(* Int syntax is generic enough: *)
(* General format: [sign] digits *)
type int_part = IntStart | Int
let int : int t =
  { printer = (fun o v -> o (string_of_int v)) ;
    scanner = (fun i o ->
      let rec loop o oo s n part =
        match part, i o 1 with
        | IntStart, "+" -> loop (o+1) oo s n Int
        | IntStart, "-" -> loop (o+1) oo (~- s) n Int
        | (IntStart|Int), d when str_is_digit d ->
          loop (o+1) (o+1) s (n*10 + digit_of d) Int
        | _ -> oo, s, n in
      let oo, s, n = loop o o 1 0 IntStart in
      if oo > o then Some (s*n, oo) else None) ;
    descr = "integer" }
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


let default v ppp =
  { printer = ppp.printer ;
    scanner = (fun i o ->
      match ppp.scanner i o with
      | None -> Some (v, o)
      | x -> x) ;
    descr = ppp.descr }
(*$= default & ~printer:(function None -> "" | Some (d, o) -> Printf.sprintf "(%d, %d)" d o)
  (Some (42,4)) (of_string (default 17 (cst "{" -+ int +- cst "}")) "{42}" 0)
  (Some (17,0)) (of_string (default 17 (cst "{" -+ int +- cst "}")) "pas glop" 0)
 *)

(* Useful for conditional printing: *)

let delayed (ppp : 'a t) : (unit -> 'a) t =
  { printer = (fun o v -> ppp.printer o (v ())) ;
    scanner = (fun i o -> ppp.scanner i o |> map (fun (x, o) -> (fun () -> x), o)) ;
    descr = ppp.descr }
(*$= delayed
  "42" (to_string (delayed int) (fun () -> 42))
 *)


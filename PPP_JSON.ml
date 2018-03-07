include PPP.Ops
(*$inject
  let id x = x
  let of_string = PPP.of_string
  let to_string = PPP.to_string
 *)

let unit : unit PPP.t = cst "null"
(*$= unit
  (Ok ((), 4)) (of_string unit "null" 0)
 *)

let char = PPP.char "\""

(* We cannot use "null" to represent nan or inf since that would turn
 * all None float options into nans *)
let float = PPP.float "\"nan\"" "\"inf\"" "\"-inf\""

let option ppp = PPP.option ~placeholder:(cst "null") ppp

let list (ppp : 'a PPP.t) : 'a list PPP.t =
  PPP.seq "list" "[" "]" "," List.fold_left List.rev ppp

let array x = list x >>: (Array.to_list, Array.of_list)

let hashtbl (pppk : 'k PPP.t) (pppv : 'v PPP.t) : ('k, 'v) Hashtbl.t PPP.t =
  (* JSON keys are restricted to strings... *)
  let pppk_ = pppk () in
  let pppk =
    if pppk_.descr = "string" then pppk
    else PPP.char_cst '"' -+ pppk +- PPP.char_cst '"' in
  PPP.hashtbl "{" "}" "," (cst ":") pppk pppv

let groupings = [ "{","}" ; "[","]" ]
let delims = [ "," ; ":" ]

let record ?extensible x =
  PPP.record ?extensible "{" "}" ":" "," groupings delims string x
let (<->) x y = PPP.sequence "; " x y
let field ?default name x = PPP.field ":" ", " ": " ?default name x

let union x = PPP.union "{" "}" ":" groupings delims string x
let (|||) x y = PPP.alternative " | " x y
let variant name x = PPP.variant ":" "" " of " name x
let none = cst "null"

let tuple_open = "["
let tuple_close = "]"
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

(* JSON wants UTF-8 chars encoded as \u007B *)

let read_next_utf8_char s o =
  let placeholder_char = Char.code '_' in
  let invalid_utf8 = placeholder_char, 1 in
  let septet o =
    let c = Char.code s.[o] in
    if c >= 0b11000000 then placeholder_char else c land 0b00111111 in
  if o >= String.length s then invalid_arg "read_next_utf8_char" ;
  let c = Char.code s.[o] in
  if c < 0b10000000 then c, 1 else
  if c < 0b11000000 then invalid_utf8 else
  if c < 0b11100000 then
    if o >= String.length s - 1 then invalid_utf8
    else (c land 0b00011111) lsl 6 + septet (o+1), 2 else
  if c < 0b11110000 then
    if o >= String.length s - 2 then invalid_utf8
    else (c land 0b00001111) lsl 12 + septet (o+1) lsl 6 + septet (o+2), 3 else
  if c < 0b11110111 then
    if o >= String.length s - 3 then invalid_utf8
    else (c land 0b00000111) lsl 18 + septet (o+1) lsl 12 + septet (o+2) lsl 6 + septet (o+3), 4 else
  invalid_utf8

let json_encoded_string s =
  (* Straightforward super slow (FIXME) *)
  let rec loop cs o =
    if o >= String.length s then
      String.concat "" (List.rev cs) else
    let uc, sz = read_next_utf8_char s o in
    assert (sz > 0) ;
    let c =
      if sz = 1 then
        match uc with
        | 0x22 -> "\\\034" (* too many backslashes confuse qtest *)
        | 0x5c -> "\092\092"
        | 0x2f -> "\\/"
        | 0x8  -> "\\b"
        | 0xc  -> "\\f"
        | 0xa  -> "\\n"
        | 0xd  -> "\\r"
        | 0x9  -> "\\t"
        | uc   ->
          assert (uc < 128 && uc >= 0) ;
          String.make 1 (Char.chr uc)
      else Printf.sprintf "\\u%04x" uc
    in
    loop (c :: cs) (o + sz)
  in
  (loop ["\""] 0) ^ "\""

let utf_bytes_of_code_point c =
  assert (c >= 0) ;
  if c < 0x80 then [Char.chr c], 1 else
  if c < 0x800 then [
    0b11000000 + (c lsr 6)  land 0b011111 |> Char.chr ;
    0b10000000 +  c         land 0b111111 |> Char.chr ], 2 else
  if c < 0x1_0000 then [
    0b11100000 + (c lsr 12) land 0b001111 |> Char.chr ;
    0b10000000 + (c lsr  6) land 0b111111 |> Char.chr ;
    0b10000000 +  c         land 0b111111 |> Char.chr ], 3 else
  if c < 0x11_0000 then [
    0b11110000 + (c lsr 24) land 0b000111 |> Char.chr ;
    0b10000000 + (c lsr 12) land 0b111111 |> Char.chr ;
    0b10000000 + (c lsr  6) land 0b111111 |> Char.chr ;
    0b10000000 +  c         land 0b111111 |> Char.chr ], 4 else
  invalid_arg "utf_bytes_of_code_point"

let hex_digit_of c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0' else
  if c >= 'a' && c <= 'f' then 10 + Char.code c - Char.code 'a' else
  if c >= 'A' && c <= 'F' then 10 + Char.code c - Char.code 'A' else
  invalid_arg "hex_digit_of"

type string_part = First | Char | BackslashStart
let string : string PPP.t =
  fun () ->
  { PPP.printer = (fun o v -> o (json_encoded_string v)) ;
    PPP.scanner = (fun i o ->
      let rec loop o l s part =
        match part, i o 1 with
        | First, "\"" -> loop (o+1) l s Char
        | First, s ->
          PPP.parse_error o ("invalid first character in JSON string: "^ s)
        | Char, "\092" -> loop (o+1) l s BackslashStart
        | Char, "\"" -> (* The only successful termination *)
          Ok (PPP.string_of l s, o+1)
        | Char, d when String.length d > 0 ->
          loop (o+1) (d.[0]::l) (s+1) Char
        | Char, s ->
          PPP.parse_error o ("invalid character in JSON string: "^ s)
        | BackslashStart, "\"" -> loop (o+1) ('"'::l) (s+1) Char
        | BackslashStart, "\092" -> loop (o+1) ('\\'::l) (s+1) Char
        | BackslashStart, "/" -> loop (o+1) ('/'::l) (s+1) Char
        | BackslashStart, "b" -> loop (o+1) ('\b'::l) (s+1) Char
        | BackslashStart, "f" -> loop (o+1) ('\013'::l) (s+1) Char
        | BackslashStart, "n" -> loop (o+1) ('\n'::l) (s+1) Char
        | BackslashStart, "r" -> loop (o+1) ('\r'::l) (s+1) Char
        | BackslashStart, "t" -> loop (o+1) ('\t'::l) (s+1) Char
        | BackslashStart, "u" ->
          let u = i (o+1) 4 in
          if String.length u < 4 then
            PPP.parse_error o "truncated utf-8 backslash-sequence" else
          let c = hex_digit_of u.[0] lsl 12 +
                  hex_digit_of u.[1] lsl  8 +
                  hex_digit_of u.[2] lsl  4 +
                  hex_digit_of u.[3] in
          let bytes, nb_bytes = utf_bytes_of_code_point c in
          loop (o+5) (List.rev_append bytes l) (s + nb_bytes) Char
        | BackslashStart, s ->
          PPP.parse_error o ("invalid escaped character in JSON string: "^ s)
      in
      try loop o [] 0 First
      with Failure _ -> PPP.parse_error o "invalid UTF-9 encoding") ;
    PPP.descr = "string" }
(*$= string & ~printer:id
   "\"glop\"" (to_string string "glop")
   "\"\"" (to_string string "")
   "\"\\r\"" (to_string string "\r")
   "\"\\u2192\"" (to_string string "→")
 *)
(*$= string & ~printer:(function Error e -> PPP.string_of_error e | Ok (s, i) -> Printf.sprintf "(%s, %d)" s i)
  (Ok ("glop", 6)) (of_string string "\"glop\"" 0)
  (Ok ("gl\bop\n", 10)) (of_string string "\"gl\\bop\\n\"" 0)
  (Ok ("→", 8)) (of_string string "\"\\u2192\"" 0)
 *)

(*$inject
  let test_id p x =
    let s = to_string p x in
    match of_string p s 0 with
    | Ok (x',_) -> if x = x' then true else (Printf.printf "intermediary string: %S\n" s; false)
    | _ -> false
  let test_id_float x =
    match of_string float (to_string float x) 0 with
    | Ok (x',_) -> abs_float (x -. x') <= 1e-5
    | _ -> false
*)
(* Q.string can generate invalid UTF-8 strings *)
(*$Q & ~count:10
  Q.int (test_id int)
  Q.float (test_id_float)
  Q.(pair printable_string (small_list int)) (test_id (pair string (list int)))
  Q.(array_of_size Gen.small_int (pair (small_list int) bool)) (test_id (array (pair (list int) bool)))
*)

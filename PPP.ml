type writer = string -> unit
type reader = int -> int -> string
type 'a printer = writer -> 'a -> unit
type 'a scanner = reader -> int -> ('a * int) option
type 'a t = { printer : 'a printer ;
              scanner : 'a scanner ;
              descr : string }

let may f = function None -> () | Some x -> f x
let map f = function None -> None | Some x -> Some (f x)

let to_string ppp v =
  let buf = Buffer.create 100 in
  let o str = Buffer.add_string buf str in
  ppp.printer o v ;
  Buffer.contents buf

let to_out_channel ppp chan v = ppp.printer (output_string chan) v
let to_stdout pp v = to_out_channel pp stdout v
let to_stderr pp v = to_out_channel pp stderr v

let string_reader s o l =
  if o + l > String.length s then
    String.sub s o (String.length s - o)
  else
    String.sub s o l

let of_string ppp s = ppp.scanner (string_reader s)

(* We want to allow non-seekable channels therefore we must keep a short
 * past-buffer in case the scanners wants to rollback a bit: *)
exception Not_implemented
let of_in_channel ppp ?(buf_capacity=4096) ic =
  let buf = Bytes.create buf_capacity
  and buf_length = ref 0
  and buf_offset = ref 0 (* offset in ic of the first byte of buf *) in
  let reader o l =
    assert (l <= buf_capacity) ;
    if o < !buf_offset then invalid_arg "Cannot backtrack that far"
    else if o > !buf_offset + !buf_length then
      raise Not_implemented (* No real reason not to allow this, but will have to read the skipped byte *)
    else (
      let already_read = !buf_offset + !buf_length - o in
      assert (already_read >= 0) ;
      let to_read = max 0 (l - already_read) in
      let read_into =
        if to_read <= buf_capacity - !buf_length then (
          !buf_length
        ) else (
          (* Need to make room *)
          let keep = buf_capacity - to_read in
          let drop = !buf_length - keep in
          Bytes.blit buf drop buf 0 keep ;
          buf_offset := !buf_offset + drop ;
          buf_length := !buf_length - drop ;
          keep
        ) in
      let read_len = try (
        really_input ic buf read_into to_read ;
        to_read
      ) with End_of_file -> 0 in
      buf_length := !buf_length + read_len ;
      (* Printf.eprintf "Buffer: length=%d, offset=%d, content=%S\n%!"
          !buf_length !buf_offset (Bytes.to_string buf) ; *)
      let str_start = o - !buf_offset in
      let str_len = min l (!buf_length - str_start) in
      Bytes.sub_string buf str_start str_len
    ) in
  ppp.scanner reader

let of_stdin ?buf_capacity ppp = of_in_channel ?buf_capacity ppp stdin

(* TODO: of_seekable_in_channel *)

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

(* C-like strings. Format: "..." *)
type string_part = First | Char | BackslashStart | Backslash
let string : string t =
  { printer = (fun o v -> o (Printf.sprintf "%S" v)) ;
    scanner = (fun i o ->
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
      loop o [] 0 0 First) ;
    descr = "string" }
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

(* C-like identifiers. Start with a letter of underscore, then can contain digits. *)
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

let next_word is_word i o =
  let rec skip_word o =
    let s = i o 1 in
    if s = "" then o else (
      let c = s.[0] in
      if is_word c then skip_word (o+1) else o
    ) in
  let o' = skip_word o in
  if o' = o then None else
  Some (i o (o'-o), o')

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
    descr = ppp1.descr ^" followed by "^ ppp2.descr }

let (-+) ppp1 ppp2 =
  { printer = (fun o v2 ->
      ppp1.printer o () ;
      ppp2.printer o v2) ;
    scanner = (fun i o ->
      match ppp1.scanner i o with
      | None -> None
      | Some ((), o) -> ppp2.scanner i o) ;
    descr = ppp1.descr ^" followed by "^ ppp2.descr }

let (+-) ppp1 ppp2 =
  { printer = (fun o v1 ->
      ppp1.printer o v1 ;
      ppp2.printer o ()) ;
    scanner = (fun i o ->
      match ppp1.scanner i o with
      | None -> None
      | Some (v, o) ->
        ppp2.scanner i o |> map (fun (_, o) -> v, o)) ;
    descr = ppp1.descr ^" followed by "^ ppp2.descr }

let (--) ppp1 ppp2 =
  { printer = (fun o _ ->
      ppp1.printer o () ;
      ppp2.printer o ()) ;
    scanner = (fun i o ->
      match ppp1.scanner i o with
      | None -> None
      | Some ((), o) ->
        ppp2.scanner i o |> map (fun (_, o) -> (), o)) ;
    descr = ppp1.descr ^" followed by "^ ppp2.descr }

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
    descr = s }

let bool : bool t =
  { printer = (fun o v -> o (if v then "true" else "false")) ;
    scanner = (fun i o ->
      match next_word_eq "true" i o with
      | false, _ ->
        (match next_word_eq "false" i o with
        | true, o -> Some (false, o)
        | _ -> None)
      | x -> Some x) ;
    descr = "boolean" }
(*$= bool & ~printer:id
  "true" (to_string bool true)
  "false" (to_string bool false)
 *)
(*$= bool
  (Some (true, 4)) (of_string bool "true" 0)
  (Some (false, 5)) (of_string bool "false" 0)
*)

(* Int syntax is generic enough: *)
(* General format: [sign] digits *)
exception IntegerOverflow
type int_part = IntStart | Int
let int64 : int64 t =
  { printer = (fun o v -> o (Int64.to_string v)) ;
    scanner = (fun i o ->
      let rec loop o oo s n part =
        match part, i o 1 with
        | IntStart, "+" -> loop (o+1) oo s n Int
        | IntStart, "-" -> loop (o+1) oo (~- s) n Int
        | (IntStart|Int), d when str_is_digit d ->
          let d = Int64.of_int (digit_of d) in
          if n >= Int64.(sub 922337203685477580L d) then
            raise IntegerOverflow ;
          loop (o+1) (o+1) s Int64.(add (mul n 10L) d) Int
        | _ -> oo, s, n in
      let oo, s, n = loop o o 1 0L IntStart in
      let n = if s < 0 then Int64.neg n else n in
      if oo > o then Some (n, oo) else None) ;
    descr = "integer" }

let int32 : int32 t = int64 >>:
  ((fun n -> Int64.of_int32 n),
   (fun n ->
      if n >= 2147483648L || n < -2147483648L then
        raise IntegerOverflow
      else Int64.to_int32 n))

let uint32 : int32 t = int64 >>:
  ((fun n -> Int64.of_int32 n),
   (fun n ->
      if n >= 4294967296L || n < 0L then
        raise IntegerOverflow
      else Int64.to_int32 n))

let max_int_L = Int64.of_int max_int
let min_int_L = Int64.of_int min_int
let int : int t = int64 >>:
  ((fun n -> Int64.of_int n),
   (fun n ->
      if n > max_int_L || n < min_int_L then
        raise IntegerOverflow
      else Int64.to_int n))
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
  { printer = (fun o v -> o (string_of_float v)) ;
    scanner = (fun i o ->
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
      else None) ;
    descr = "float" }
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

let optional ppp =
  { printer = (fun o -> function None -> () | Some x -> ppp.printer o x) ;
    scanner = (fun i o ->
      match ppp.scanner i o with
      | Some (x, o') -> Some (Some x, o')
      | None -> Some (None, o)) ;
    descr = "optional "^ ppp.descr }
(*$= optional & ~printer:(function None -> "" | Some (None, _) -> Printf.sprintf "none" | Some (Some d, o) -> Printf.sprintf "(%d, %d)" d o)
  (Some (Some 42,4)) (of_string (optional (cst "{" -+ int +- cst "}")) "{42}" 0)
  (Some (None,0)) (of_string (optional (cst "{" -+ int +- cst "}")) "pas glop" 0)
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

(* Generic record/union like facility *)

(* Skip until end of string. Used by skip_any. *)
let rec skip_string ?(backslashed=false) i o =
  match i o 1 with
  | "\"" -> if backslashed then skip_string i (o+1) else Some (o+1)
  (* This is tab ; avoids confusing qtest: *)
  | "\x5c" -> skip_string ~backslashed:(not backslashed) i (o + 1)
  | "" -> None
  | _ -> skip_string i (o+1)

let rec skip_delim ?(had_delim=false) i o =
  match i o 1 with
  | ("," | ";") when not had_delim -> skip_delim ~had_delim:true i (o+1)
  | s when String.length s > 0 && is_blank s.[0] -> skip_delim i (o+1)
  | _ -> if had_delim then Some o else None

let starts_with pref str =
  let len = String.length pref in
  try (
    for i = 0 to len-1 do
      if pref.[i] <> str.[i] then raise Exit
    done ;
    true
  ) with Exit -> false

(* Rewrite this crap with the help of a function that reads an identifier, an
 * eq sign, then any value until either some sep or some cls. Each of eq, sep
 * and cls have to be a string that can be empty. Then union and record are
 * easier to write.  Maybe we could just change skip_any in such a way that the
 * callback would return both the identifier and the value? *)

let stream_starts_with i o s =
  let l = String.length s in
  let str = i o l in
  str = s

(* To be able to "reorder" records fields we need a function able to tell us
 * the length of a value, without knowing its type. If we have this, then we
 * can read a sequence of identifier * anything, and deal with it.
 * Delims are used to know where the value ends. *)
let rec skip_any groupings delims i o =
  (* also handle strings *)
  let o = skip_blanks i o in
  let c = i o 1 in
  if c = "" then Some o
  else if c = "\"" then skip_string i (o+1)
  else if List.exists (stream_starts_with i o) delims then Some o
  (* Ok but this value can be itself in an outside group so group ends
   * must count as delimiters: *)
  else if List.exists (fun (_, cls) -> stream_starts_with i o cls) groupings then Some o
  else (
    match List.find (fun (opn, _) ->
      stream_starts_with i o opn) groupings with
    | exception Not_found ->
      (* Not a group: this char is part of the value that we skip *)
      skip_any groupings delims i (o+1)
    | opn, cls ->
      skip_group cls groupings i (o + String.length opn)
  )
and skip_group cls groupings i o =
  let clsl = String.length cls in
  let o = skip_blanks i o in
  if i o clsl = cls then Some (o + clsl) else
  match skip_any groupings [cls] i o with
  | None -> None
  | Some o' ->
    let o'' = skip_blanks i o' in
    let s = i o'' clsl in
    if s = cls then (
      Some (o'' + clsl)
    ) else None
(*$= skip_any & ~printer:(function None -> "" | Some o -> string_of_int o)
  (Some 4) (skip_any [] [","] (string_reader "glop") 0)
  (Some 6) (skip_any ["(",")"] [] (string_reader "(glop)") 0)
  (Some 8) (skip_any ["[|","|]"] [] (string_reader "[|glop|]") 0)
  (Some 2) (skip_any ["(",")"] [] (string_reader "()") 0)
  (Some 7) (skip_any ["[","]"] [";"] (string_reader "[1;2;3] ; glop") 0)
  (Some 13) (skip_any ["[","]"] [";"] (string_reader "[ 1 ; 2 ; 3 ]") 0)
  (Some 12) (skip_any ["(",")"] [" "] (string_reader " \"b()l\\\"a][\" z") 0)
  (Some 26) (skip_any ["{","}"] [";"] (string_reader "{ a = 43 ; glop=(1, 2 ); } ; z ") 0)
  None (skip_any ["{","}" ; "(",")"] [" ";";"] (string_reader "{ a = 43 ; glop=1 ) ") 0)
 *)

let debug = Printf.ifprintf

(* Instead of a normal ppp, the printers used by union take
 * a optional value and prints only when it's set - with the idea
 * that this value will be a pair of pairs of pairs obtained with |||.
 *
 * The scanners take a string (the name of the field) and a reader that
 * will read the string value.
 * Then the ||| combinator will work on an optional pair of optional pair
 * of... and print/scan only one value (suitable for enum) while the
 * <-> combinator will work on pairs of pairs and set one of the item
 * based on the name. In both case the field combinator returns an
 * optional value depending if the field name match or not. *)
type 'a u =
  (string t -> bool -> 'a printer) *    (* name ppp, bool is: do we need a separator? *)
  (string -> 'a scanner) *  (* string is: field value that's going to be read *)
  string                    (* how to build the type name *)
let union opn cls eq groupings delims name_ppp (p, s, descr : 'a u) : 'a t =
  { printer = (fun o x ->
      o opn ;
      p name_ppp false o x ;
      o cls) ;
    scanner = (fun i o ->
      let opn_len = String.length opn
      and cls_len = String.length cls in
      let o = skip_blanks i o in
      match i o opn_len with
      | str when str = opn ->
        debug stderr "found union opn\n" ;
        let o = o + opn_len in
        (match name_ppp.scanner i o with
        | None -> None
        | Some (name, o') ->
          debug stderr "found union name '%s'\n" name ;
          (* Now skip the eq sign *)
          let o' = skip_blanks i o' in
          let eq_len = String.length eq in
          (match i o' eq_len with
          | e when e = eq ->
            debug stderr "found eq '%s'\n" eq ;
            let o' = skip_blanks i (o' + eq_len) in
            (* Now the value *)
            let delims' = if cls_len > 0 then cls::delims else delims in
            (match skip_any groupings delims' i o' with
            | None -> None
            | Some o ->
              let o = skip_blanks i o in
              (* Check we have cls: *)
              (match i o cls_len with
              | str when str = cls ->
                debug stderr "found cls '%s'\n" cls ;
                let value = chop_sub (i o' (o-o')) 0 (o-o') in
                let i = string_reader value in
                (match s name i 0 with
                | Some (v, _o') ->
                  debug stderr "found value and parsed it.\n" ;
                  (* TODO: check o' *)
                  Some (v, o + cls_len)
                | x -> x)
              | _ -> None))
          | _ -> None))
      | _ -> None) ;
    descr }

(* Combine two ppp into a pair of options *)
let alternative var_sep (p1, s1, id1 : 'a u) (p2, s2, id2 : 'b u) : ('a option * 'b option) u =
  (fun name_ppp need_sep o (v1,v2) ->
    may (p1 name_ppp need_sep o) v1 ;
    let need_sep = need_sep || v1 <> None in
    may (p2 name_ppp need_sep o) v2),
  (fun n i o ->
    match s1 n i o with
    | Some (x, o) -> Some ((Some x, None), o)
    | None ->
      (match s2 n i o with
      | Some (x, o) -> Some ((None, Some x), o)
      | None -> None)),
  (id1 ^ var_sep ^ id2)

let variant eq sep id_sep name (ppp : 'a t) : 'a u =
  (fun name_ppp need_sep o v ->
    if need_sep then o sep ;
    name_ppp.printer o name ;
    o eq ;
    ppp.printer o v),
  (fun n i o ->
    if n <> name then None
    else ppp.scanner i o),
  (name ^ id_sep ^ ppp.descr)

(* Like unit but with no representation, useful for
 * constructor without arguments *)
let none : unit t =
  { printer = (fun _o () -> ()) ;
    scanner = (fun _i o -> Some ((), o)) ;
    descr = "" }
(*$= none
  (Some ((), 0)) (of_string none "" 0)
 *)


(*$inject
  let groupings = [ "(",")" ; "[","]" ; "[|","|]" ]
  let delims = [ "," ; ";" ]
  type color = RGB of int * int * int
             | Named of string
             | Transparent

  let first ppp = cst "(" -+ ppp
  let last ppp = ppp +- cst ")"
  let sep = cst ","
  let tuple3 (p1 : 'a t) (p2 : 'b t) (p3 : 'c t) : ('a * 'b * 'c) t =
    first p1 +- sep ++ p2 +- sep ++ last p3 >>:
      ((fun (v1,v2,v3) -> (v1,v2),v3),
       (fun ((v1,v2),v3) -> v1,v2,v3))
  let (|||) x y = alternative " | " x y

  let color : color t = union "" "" "" groupings delims identifier (
    variant " " "" " of " "RGB" (tuple3 int int int) |||
    variant " " "" " of " "Named" string |||
    variant "" "" " of " "Transp" none) >>:
    ((function RGB (r,g,b) -> Some (Some (r,g,b), None), None
             | Named name -> Some (None, Some name), None
             | Transparent -> None, Some ()),
     (function Some (Some (r,g,b), _), _ -> RGB (r,g,b)
             | Some (_, Some name), _ -> Named name
             | _ -> Transparent))
 *)
(*$= color & ~printer:id
  "RGB (0,0,255)" (to_string color (RGB (0, 0, 255)))
  "Transp" (to_string color Transparent)
 *)
(*$= color & ~printer:(function None -> "" | Some (p, o) -> Printf.sprintf "(%s, %d)" (to_string color p) o)
  (Some (RGB (0,0,255), 12)) (of_string color "RGB(0,0,255)" 0)
  (Some (RGB (0,0,255), 13)) (of_string color "RGB (0,0,255)" 0)
  (Some (RGB (0,0,255), 15)) (of_string color "  RGB (0,0,255)" 0)
  (Some (RGB (0,0,255), 22)) (of_string color "RGB  ( 0 ,  0, 255  ) " 0)
  (Some (Transparent, 8)) (of_string color " Transp " 0)
 *)

exception MissingRequiredField

(* When using union for building a record, aggregate the tree of pairs at
 * every field.  This merge has to be recursive so we must build the merge
 * function as we build the total tree of pairs with <->. *)
type 'a merge_u = 'a option -> 'a option -> 'a option
let record opn cls eq sep groupings delims name_ppp ((p, s, descr : 'a u), (merge : 'a merge_u)) =
  (* In a record we assume there are nothing special to open/close a single
   * field. If not, add 2 parameters in the record: *)
  let nf = union "" "" eq groupings (cls::delims) name_ppp (p, s, descr) in
  { printer = (fun o v ->
      o opn ;
      p name_ppp false o v ;
      o cls) ;
    scanner = (fun i o ->
      let o = skip_blanks i o in
      let opn_len = String.length opn in
      let sep_len = String.length sep in
      let cls_len = String.length cls in
      match i o opn_len with
      | str when str = opn ->
        let o = skip_blanks i (o + opn_len) in
        let rec loop prev o =
          match nf.scanner i o with
          | None -> prev, o
          | Some (x, o) ->
            let prev' = merge prev (Some x) in
            let o = skip_blanks i o in
            (match i o sep_len with
            | str when str = sep ->
              loop prev' (o + sep_len)
            | _ -> prev', o) in
        let res, o = loop None o in
        (* Check we are done *)
        let o = skip_blanks i o in
        (match i o cls_len with
        | str when str = cls ->
          (match res with
          | None -> None
          | Some r -> Some (r, o + cls_len))
        | _ -> None)
      | _ -> None) ;
    descr = opn ^ descr ^ cls}

(* But then we need a special combinator to also compute the merge of the pairs of pairs... *)
let sequence field_sep ((psi1 : 'a u), (m1 : 'a merge_u)) ((psi2 : 'b u), (m2 : 'b merge_u)) : (('a option * 'b option) u * ('a option * 'b option) merge_u) =
  (alternative field_sep psi1 psi2),
  (fun (v1 : ('a option * 'b option) option) (v2 : ('a option * 'b option) option) ->
    if v1 = None && v2 = None then None else
    Some (
      let map_get f = function
        | None -> None
        | Some x -> f x in
      m1 (map_get fst v1) (map_get fst v2),
      m2 (map_get snd v1) (map_get snd v2)))

let field eq sep id_sep ?default name (ppp : 'a t) : ('a u * 'a merge_u) =
  (variant eq sep id_sep name ppp),
  (fun r1 r2 ->
    if r2 = None then (
      if r1 = None then default else r1
    ) else r2)

(*$inject
  type person = { name: string ; age: int ; male: bool }
  let (<->) x y = sequence "; " x y

  let person : person t =
    record "{" "}" "=" ";" groupings delims identifier (
      field "=" "; " ": " "name" string <->
      field "=" "; " ": " "age" int <->
      field "=" "; " ": " ~default:true "male" bool) >>:
      ((fun { name ; age ; male } -> Some (Some name, Some age), Some male),
       (function (Some (Some name, Some age), Some male) -> { name ; age ; male }
               | _ -> raise MissingRequiredField))
 *)
(*$= person & ~printer:id
  "{name=\"John\"; age=41; male=true}" (to_string person { name = "John"; age = 41; male = true })
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

let to_unit def ppp = ppp >>: ((fun _ -> def), ignore)

let newline =
  to_unit None (optional (cst "\r")) -- cst "\n"

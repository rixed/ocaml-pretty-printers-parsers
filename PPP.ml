open Stdint

let debug = false
let trace fmt =
  if debug then Printf.fprintf stderr (fmt ^^ "\n%!")
  else Printf.ifprintf stderr fmt

exception NotImplemented
exception CannotBacktrackThatFar
exception IntegerOverflow
exception MissingRequiredField of string (* used by ppx *)

type error_type = CannotParse of string
                | UnknownField of string * int (* end of value *)
                | NotDone of string
type error = int * error_type

let parse_error o s = Error (o, CannotParse s)
let unknown_field o1 o2 n = Error (o1, UnknownField (n, o2))
let not_done o s = Error (o, NotDone s)

let string_of_error ?(location_of_offset=string_of_int) (o, e) =
  match e with
  | CannotParse s -> "Parse error at "^ location_of_offset o ^": "^ s
  | UnknownField (f, _) -> "Unknown field '"^ f ^"' at "^ location_of_offset o
  | NotDone s -> "Was expecting "^ s ^" at end of input"

(* Favor e1 in case of draw *)
let best_error (o1, _ as e1) (o2, _ as e2) =
  trace "best_error: comparing %s and %s"
    (string_of_error e1) (string_of_error e2) ;
  if o2 > o1 then e2 else e1

(* Some helpers: *)

let is_letter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\b'
let zero = Char.code '0'
let str_is_digit s = String.length s > 0 && is_digit s.[0]
let digit_of s = Char.code s.[0] - zero

let chop_sub s b e =
  let len = String.length s in
  if e > len || b > len || e < 0 || b < 0 then
    Printf.sprintf "chop_sub %S %d %d" s b e |>
    invalid_arg ;
  (* Also return the nb of chars chopped at the beginning. Useful later to
   * compute offset in the user provided string from the offset in chopped
   * string. *)
  let rec aux b e =
    if b >= e then b, "" else
    if is_blank s.[b] then aux (b+1) e else
    if is_blank s.[e-1] then aux b (e-1) else
    b, String.sub s b (e-b)
  in
  let b', s = aux b e in
  b' - b, s
(*$inject let id x = x *)
(*$= chop_sub & ~printer:(fun (o, s) -> Printf.sprintf "(%d, %S)" o s)
  (0, "glop") (chop_sub "glop" 0 4)
  (1, "glop") (chop_sub " glop" 0 5)
  (0, "glop") (chop_sub "glop " 0 5)
  (0, "glop") (chop_sub "glop	" 0 5)
  (2, "glop") (chop_sub "  glop" 0 6)
  (3, "glop") (chop_sub "	 	glop " 0 8)
  (0, "") (chop_sub "" 0 0)
  (1, "") (chop_sub " " 0 1)
  (0, "") (chop_sub " " 1 1)
*)

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

let starts_with pref str =
  let len = String.length pref in
  if String.length pref > len then false else
  try (
    for i = 0 to len-1 do
      if pref.[i] <> str.[i] then raise Exit
    done ;
    true
  ) with Exit -> false

let stream_starts_with i o s =
  let l = String.length s in
  let str = i o l in str = s

let may f = function None -> () | Some x -> f x

type writer = string -> unit
type reader = int -> int -> string
type 'a printer = writer -> 'a -> unit
type 'a scanner = reader -> int -> (('a * int), error) result
(* We use 'a r but we define and take as argument 'a t, so that we can write
 * mutually recursive definitions such as:
 * let ppp1 () = ... ppp2 ... and ppp2 () = ... ppp1 ... *)
(* Note re. recursive types:
 * When parsing and printing we always make progress along the recursion,
 * but when printing the type name we would loop forever ("T1 of T2 of T1
 * of..."). To avoid this, the descr will take a depth as a parameter. *)
type 'a r = { printer : 'a printer ;
              scanner : 'a scanner ;
              descr : int -> string }
type 'a t = unit -> 'a r

let rec skip_blanks i o =
  let s = i o 1 in
  if String.length s = 1 && is_blank s.[0] then skip_blanks i (o + 1)
  else o

(* Let's start with a PPP for no value, that swallows blanks: *)
let blanks : unit t =
  fun () ->
  { printer = (fun _o () -> ()) ;
    scanner = (fun i o ->
      let o = skip_blanks i o in
      Ok ((), o)) ;
    descr = fun _ -> "" }

(* Some combinators: *)

let (++) ppp1 ppp2 =
  fun () ->
  let ppp1_ = ppp1 () and ppp2_ = ppp2 () in
  { printer = (fun o (v1, v2) ->
      ppp1_.printer o v1 ;
      ppp2_.printer o v2) ;
    scanner = (fun i o ->
      match ppp1_.scanner i o with
      | Error _ as e -> e
      | Ok (v1, o) ->
        (match ppp2_.scanner i o with
        | Error _ as e -> e
        | Ok (v2, o) -> Ok ((v1, v2), o))) ;
    descr = fun depth  -> ppp1_.descr (depth + 1) ^ ppp2_.descr (depth + 1) }

let (-+) ppp1 ppp2 =
  fun () ->
  let ppp1_ = ppp1 () and ppp2_ = ppp2 () in
  { printer = (fun o v2 ->
      ppp1_.printer o () ;
      ppp2_.printer o v2) ;
    scanner = (fun i o ->
      match ppp1_.scanner i o with
      | Error _ as e -> e
      | Ok ((), o) -> ppp2_.scanner i o) ;
    descr = fun depth -> ppp1_.descr (depth + 1) ^ ppp2_.descr (depth + 1) }

let (+-) ppp1 ppp2 =
  fun () ->
  let ppp1_ = ppp1 () and ppp2_ = ppp2 () in
  { printer = (fun o v1 ->
      ppp1_.printer o v1 ;
      ppp2_.printer o ()) ;
    scanner = (fun i o ->
      match ppp1_.scanner i o with
      | Error _ as e -> e
      | Ok (v, o) ->
        (match ppp2_.scanner i o with
        | Error _ as e -> e
        | Ok (_, o) -> Ok (v, o))) ;
    descr = fun depth -> ppp1_.descr (depth + 1) ^ ppp2_.descr (depth + 1) }

let (--) ppp1 ppp2 =
  fun () ->
  let ppp1_ = ppp1 () and ppp2_ = ppp2 () in
  { printer = (fun o _ ->
      ppp1_.printer o () ;
      ppp2_.printer o ()) ;
    scanner = (fun i o ->
      match ppp1_.scanner i o with
      | Error _ as e -> e
      | Ok ((), o) ->
        (match ppp2_.scanner i o with
        | Error _ as e -> e
        | Ok (_, o) -> Ok ((), o))) ;
    descr = fun depth -> ppp1_.descr (depth + 1) ^ ppp2_.descr (depth + 1) }

let (>>:) ppp (f,f') =
  fun () ->
  let ppp_ = ppp () in
  { printer = (fun o v -> ppp_.printer o (f v)) ;
    scanner = (fun i o ->
      match ppp_.scanner i o with
      | Error _ as e -> e
      | Ok (x, o) -> Ok (f' x, o)) ;
    descr = ppp_.descr }


(* Various I/O functions *)

let to_string ?(pretty=false) ppp v =
  let buf = Buffer.create 100 in
  let o str = Buffer.add_string buf str in
  let ppp_ = ppp () in
  ppp_.printer o v ;
  let s = Buffer.contents buf in
  if pretty then PPP_prettify.prettify s else s

let to_out_channel ?(pretty=false) ppp chan v =
  let ppp_ = ppp () in
  if pretty then
    to_string ~pretty ppp v |>
    output_string chan
  else
    ppp_.printer (output_string chan) v

let to_stdout ?pretty ppp v = to_out_channel ?pretty ppp stdout v
let to_stderr ?pretty ppp v = to_out_channel ?pretty ppp stderr v

(* if this string is part of another string we might want to offset the
 * locations to pretend we are reading the original string: *)
let string_reader ?(offset=0) s o l =
  let o = o - offset in
  if o >= String.length s then "" else
  let len =
    if o + l > String.length s then String.length s - o else l in
  String.sub s o len

let of_string ppp s =
  let ppp_ = (ppp +- blanks) () in
  ppp_.scanner (string_reader s)

let chop s = chop_sub s 0 (String.length s)

exception ParseError of string
exception TrailingGarbage of int

let () =
  Printexc.register_printer (function
    | ParseError s ->
        Some s
    | TrailingGarbage o ->
        Some ("Trailing content at offset "^ string_of_int o)
    | _ -> None)

let location_of_offset o s =
  let rec loop l c i =
    if i >= o then Printf.sprintf "line %d, column %d" (l+1) (c+1)
    else if s.[i] = '\n' then loop (l+1) 0 (i+1)
    else loop l (c+1) (i+1)
  in
  loop 0 0 0

let of_string_exc ppp s =
  let l, s' = chop s in
  let location_of_offset o =
    location_of_offset (o + l) s in
  match of_string ppp s' 0 with
  | Error e -> raise (ParseError (string_of_error ~location_of_offset e))
  | Ok (_, l) when l < String.length s' -> raise (TrailingGarbage l)
  | Ok (x, _) -> x

(* TODO: get rid of that one *)
let of_string_res ppp s =
  let _, s = chop s in
  of_string ppp s 0

(* We want to allow non-seekable channels therefore we must keep a short
 * past-buffer in case the scanners wants to rollback a bit: *)
let of_non_seekable_in_channel ?(buf_capacity=4096) ppp ic =
  let buf = Bytes.create buf_capacity
  and buf_length = ref 0
  and buf_offset = ref 0 (* offset in ic of the first byte of buf *) in
  let rec reader o l =
    assert (l <= buf_capacity) ;
    if o < !buf_offset then raise CannotBacktrackThatFar else
    (* No real reason not to allow this, but will have to read the skipped bytes*)
    if o > !buf_offset + !buf_length then raise NotImplemented else (
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
      let read_or_zero ic buf ofs len =
        try really_input ic buf ofs len ; len
        with End_of_file -> 0 in
      let read_len = read_or_zero ic buf read_into to_read in
      buf_length := !buf_length + read_len ;
      (* Printf.eprintf "Buffer: length=%d, offset=%d, content=%S\n%!"
          !buf_length !buf_offset (Bytes.to_string buf) ; *)
      (* We are allowed to return a smaller string only in case of EOF: *)
      let str_start = o - !buf_offset in
      let str_len = min l (!buf_length - str_start) in
      if str_len < l && read_len > 0 then
        reader o l (* Keep reading *)
      else
        Bytes.sub_string buf str_start str_len
    )
  in
  let ppp_ = ppp () in
  ppp_.scanner reader

let of_seekable_in_channel ppp ic =
  let cur_offset = ref 0 in
  let reader o l =
    if o <> !cur_offset then (
      assert (o < !cur_offset) ;
      assert (o >= 0) ;
      seek_in ic o ;
      cur_offset := o
    ) ;
    let buf = Bytes.create l in
    let rec read_or_zero ic pos =
      let rem = Bytes.length buf - pos in
      if rem <= 0 then pos else
        match input ic buf pos rem with
        | 0 (*end of file *) -> pos
        | l ->
            cur_offset := !cur_offset + l ;
            read_or_zero ic (pos + l) in
    let len = read_or_zero ic 0 in
    Bytes.sub_string buf 0 len
  in
  let ppp_ = ppp () in
  ppp_.scanner reader

let of_in_channel ?buf_capacity ppp ic =
  let ppp = ppp +- blanks in
  (* If this is not seekable then we will need a buffer: *)
  match seek_in ic 0 with
  | exception _ ->
    of_non_seekable_in_channel ?buf_capacity ppp ic
  | _ ->
    of_seekable_in_channel ppp ic

let of_in_channel_exc ?buf_capacity ppp ic =
  match of_in_channel ?buf_capacity ppp ic 0 with
  | Error e -> raise (ParseError (string_of_error e))
  | Ok (_, l) when l < in_channel_length ic -> raise (TrailingGarbage l)
  | Ok (x, _) -> x

let of_stdin ?buf_capacity ppp =
  of_in_channel ?buf_capacity ppp stdin

let next_eq w i o =
  if stream_starts_with i o w then true, o + String.length w
  else false, o

(* Parse a given char - useful with combinators *)
let char_cst c : unit t =
  let s = String.make 1 c in
  fun () ->
  { printer = (fun o () -> o s) ;
    scanner = (fun i o ->
      if i o 1 <> s then parse_error o ("Missing constant char "^ s)
      else Ok ((), o + 1)) ;
    descr = fun _ -> s }

let char quote : char t =
  fun () ->
  { printer = (fun o v -> o quote ; o (String.make 1 v) ; o quote) ;
    scanner = (fun i o ->
      let ql = String.length quote in
      if i o ql <> quote then parse_error o "Missing opening quote" else
      let o = o + ql in
      match i o 1 with
      | "" -> not_done o "character"
      | s ->
        let o = o + 1 in
        if i o ql <> quote then parse_error o "Missing closing quote" else
        Ok (s.[0], o+ql)) ;
    descr = fun _ -> "char" }

(* C-like strings. Format: "..." *)
type string_part = First | Char | BackslashStart | Backslash
let string : string t =
  fun () ->
  { printer = (fun o v -> o (Printf.sprintf "%S" v)) ;
    scanner = (fun i o ->
      let rec loop o l s bsn part =
        match part, i o 1 with
        | First, "\"" -> loop (o+1) l s bsn Char
        | Char, "\\" -> loop (o+1) l s bsn BackslashStart
        | Char, "\"" -> (* The only successful termination *)
          Ok (string_of l s, o+1)
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
              if bsn > 255 then parse_error o "Invalid backslash sequence in string"
              else loop (o+1) (Char.chr bsn :: l) (s+1) 0 Char
            ) else (
              (* 10+ so that we know when we have had 3 digits: *)
              loop (o+1) l s (10*bsn + digit_of d) Backslash
            )
        | _ -> parse_error o "Invalid character in string" (* everything else is game-over *)
      in
      loop o [] 0 0 First) ;
    descr = fun _ -> "string" }
(*$= string & ~printer:id
   "\"glop\"" (to_string string "glop")
   "\"\"" (to_string string "")
   "\"\\207\"" (to_string string "\207")
 *)
(*$= string & ~printer:(function Error e -> string_of_error e | Ok (s, i) -> Printf.sprintf "(%s, %d)" s i)
  (Ok ("glop", 6)) (of_string string "\"glop\"" 0)
  (Ok ("gl\bop\n", 10)) (of_string string "\"gl\\bop\\n\"" 0)
  (Ok ("\207", 6)) (of_string string "\"\\207\"" 0)
 *)

(* C-like identifiers. Start with a letter or underscore, then can contain digits.
 * As a nice addition, can also contains minus sign (frequently used in place
 * of spaces, as are underscores) *)
let identifier : string t =
  fun () ->
  { printer = (fun o x -> o x) ;
    scanner = (fun i o ->
      let rec loop oo =
        let s = i oo 1 in
        if s = "" then oo else
        let c = s.[0] in
        if is_letter c || c = '_' || oo > o && (is_digit c || c = '-') then
          loop (oo+1)
        else
          oo
      in
      let oo = loop o in
      if oo > o then Ok (i o (oo-o), oo) else parse_error o "Identifier") ;
    descr = fun _ -> "identifier" }
(*$= identifier & ~printer:(function Error e -> string_of_error e | Ok (i, o) -> Printf.sprintf "(%s,%d)" i o)
  (Ok ("glop", 4)) (of_string identifier "glop" 0)
  (Ok ("glop", 5)) (of_string identifier "glop\n" 0)
  (Ok ("glop0", 5)) (of_string identifier "glop0" 0)
*)
(*$inject let is_error = function Error _ -> true | _ -> false *)
(*$T identifier
  is_error (of_string identifier "0glop" 0)
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
    if str_is_digit s then
      loop (n*10 + digit_of s) (o+1)
    else n, o
  in
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

let until_any us i o =
  let mi, ma =
    List.fold_left (fun (mi, ma) u ->
      let l = String.length u in
      min mi l, max ma l) (max_int, min_int) us in
  let rec loop o =
    let s = i o ma in
      if String.length s >= mi &&
         List.exists (fun u -> starts_with u s) us then Some o else
      if String.length s < mi then None else
      loop (o + 1)
  in
  loop o

let next_word is_word i o =
  let rec skip_word o =
    let s = i o 1 in
    if s = "" then o else (
      let c = s.[0] in
      if is_word c then skip_word (o+1) else o
    )
  in
  let o' = skip_word o in
  if o' = o then None else Some (i o (o'-o), o')

let seq name opn cls sep fold of_rev_list ppp =
  fun () ->
  let ppp_ = ppp () in
  { printer = (fun o v ->
      o opn ;
      fold (fun i v' ->
          if i > 0 then o sep ;
          ppp_.printer o v' ;
          i + 1) 0 v ;
      o cls) ;
    scanner = (fun i o ->
      trace "seq: trying to parse a sequence from %d" o ;
      let rec parse_sep prev o =
        let o = skip_blanks i o in
        if stream_starts_with i o sep then
          parse_item prev (o + String.length sep)
        else if stream_starts_with i o cls then
          Ok (of_rev_list prev, o + String.length cls)
        else (
          trace "seq: cannot find separator %S at %d" sep o ;
          parse_error o (Printf.sprintf "Cannot find separator %S" sep))
      and parse_item prev o =
        let o = skip_blanks i o in
        trace "seq: skipped blanks, first item starts at %d with %S" o (i o 1) ;
        match ppp_.scanner i o with
        | Ok (x, o) ->
          trace "seq: found item, now looking for sep %S" sep ;
          parse_sep (x::prev) o
        | Error (oo, _) as err ->
          if stream_starts_with i o cls then
            Ok (of_rev_list prev, o + String.length cls)
          else (
            trace "seq: cannot parse item at %d (err at %d)" o oo ;
            err)
      in
      let o = skip_blanks i o in
      if stream_starts_with i o opn then (
        trace "seq: found opn %S at %d" opn o ;
        parse_item [] (o + String.length opn)
      ) else (
        trace "seq: cannot find opn %S" opn ;
        let err = Printf.sprintf "Cannot find sequence opening %S" opn in
        parse_error o err)) ;
    descr = fun depth -> name ^" of "^ ppp_.descr (depth + 1) }
(*$= seq & ~printer:(function Error e -> string_of_error e | Ok (l,o) -> Printf.sprintf "(%s, %d)" (String.concat ";" l) o)
  (Ok (["a";"b";"cde"], 9)) \
    (of_string (seq "list" "[" "]" ";" List.fold_left List.rev identifier) "[a;b;cde]" 0)
  (Ok (["a";"b";"cde"], 9)) \
    (of_string (seq "sequence" "" "" "==" List.fold_left List.rev identifier) "a==b==cde" 0)
 *)

(* Always allow blanks around the constant *)
let cst s : unit t =
  fun () ->
  { printer = (fun o () -> o s) ;
    scanner = (fun i o ->
      let o = skip_blanks i o in
      let l = String.length s in
      if stream_starts_with i o s then
        let o = skip_blanks i (o + l) in
        Ok ((), o)
      else parse_error o (Printf.sprintf "Cannot find %S" s)) ;
    descr = fun _ -> s }

let bool : bool t =
  fun () ->
  { printer = (fun o v -> o (if v then "true" else "false")) ;
    scanner = (fun i o ->
      match next_word_eq "true" i o with
      | true, _ as x -> Ok x
      | _ ->
        (match next_word_eq "false" i o with
        | true, o -> Ok (false, o)
        | _ -> parse_error o "Expected boolean")) ;
    descr = fun _ -> "boolean" }
(*$= bool & ~printer:id
  "true" (to_string bool true)
  "false" (to_string bool false)
 *)
(*$= bool
  (Ok (true, 4)) (of_string bool "true" 0)
  (Ok (false, 5)) (of_string bool "false" 0)
*)

(* Int syntax is generic enough: *)
(* General format: [sign] digits *)
type int_part = IntStart | Int

(* TODO: support for base changing prefix? *)
let generic_int_scanner of_int add mul zero neg i o =
  let rec loop o oo s n part =
    match part, i o 1 with
    | IntStart, "+" -> loop (o+1) oo s n Int
    | IntStart, "-" -> loop (o+1) oo (~- s) n Int
    | (IntStart|Int), d when str_is_digit d ->
        let d = of_int (digit_of d) in
        loop (o+1) (o+1) s (add (mul n (of_int 10)) d) Int
    | _ -> oo, s, n
  in
  let oo, s, n = loop o o 1 zero IntStart in
  let n = if s < 0 then neg n else n in
  if oo > o then Ok (n, oo) else parse_error o "Expected integer"

let int128 : int128 t =
  fun () ->
  { printer = (fun o v -> o (Int128.to_string v)) ;
    scanner = (let open Int128 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int128" }

let uint128 : uint128 t =
  fun () ->
  { printer = (fun o v -> o (Uint128.to_string v)) ;
    scanner = (let open Uint128 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint128" }

let int64 : int64 t =
  fun () ->
  { printer = (fun o v -> o (Int64.to_string v)) ;
    scanner = (let open Int64 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int64" }

let uint64 : uint64 t =
  fun () ->
  { printer = (fun o v -> o (Uint64.to_string v)) ;
    scanner = (let open Uint64 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint64" }

let int56 : int56 t =
  fun () ->
  { printer = (fun o v -> o (Int56.to_string v)) ;
    scanner = (let open Int56 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int56" }

let uint56 : uint56 t =
  fun () ->
  { printer = (fun o v -> o (Uint56.to_string v)) ;
    scanner = (let open Uint56 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint56" }

let int48 : int48 t =
  fun () ->
  { printer = (fun o v -> o (Int48.to_string v)) ;
    scanner = (let open Int48 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int48" }

let uint48 : uint48 t =
  fun () ->
  { printer = (fun o v -> o (Uint48.to_string v)) ;
    scanner = (let open Uint48 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint48" }

let int40 : int40 t =
  fun () ->
  { printer = (fun o v -> o (Int40.to_string v)) ;
    scanner = (let open Int40 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int40" }

let uint40 : uint40 t =
  fun () ->
  { printer = (fun o v -> o (Uint40.to_string v)) ;
    scanner = (let open Uint40 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint40" }

let int32 : int32 t =
  fun () ->
  { printer = (fun o v -> o (Int32.to_string v)) ;
    scanner = (let open Int32 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int32" }

let uint32 : uint32 t =
  fun () ->
  { printer = (fun o v -> o (Uint32.to_string v)) ;
    scanner = (let open Uint32 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint32" }

let int24 : int24 t =
  fun () ->
  { printer = (fun o v -> o (Int24.to_string v)) ;
    scanner = (let open Int24 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int24" }

let uint24 : uint24 t =
  fun () ->
  { printer = (fun o v -> o (Uint24.to_string v)) ;
    scanner = (let open Uint24 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint24" }

let int16 : int16 t =
  fun () ->
  { printer = (fun o v -> o (Int16.to_string v)) ;
    scanner = (let open Int16 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int16" }

let uint16 : uint16 t =
  fun () ->
  { printer = (fun o v -> o (Uint16.to_string v)) ;
    scanner = (let open Uint16 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint16" }

let int8 : int8 t =
  fun () ->
  { printer = (fun o v -> o (Int8.to_string v)) ;
    scanner = (let open Int8 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "int8" }

let uint8 : uint8 t =
  fun () ->
  { printer = (fun o v -> o (Uint8.to_string v)) ;
    scanner = (let open Uint8 in generic_int_scanner of_int add mul zero neg) ;
    descr = fun _ -> "uint8" }

let int : int t =
  fun () ->
  { printer = (fun o v -> o (string_of_int v)) ;
    scanner = generic_int_scanner (fun x:int -> x) (+) ( * ) 0 (~-) ;
    descr = fun _ -> "int" }
(*$= int & ~printer:id
  "42" (to_string int 42)
  "-42" (to_string int (-42))
  "0" (to_string int 0)
 *)
(*$= int & ~printer:(function Error e -> string_of_error e | Ok (v,i) -> Printf.sprintf "(%d, %d)" v i)
  (Ok (42, 2)) (of_string int "42" 0)
  (Ok (-42, 3)) (of_string int "-42" 0)
  (Ok (0, 1)) (of_string int "0" 0)
  (Ok (5, 1)) (of_string int "5glop" 0)
  (Ok (0, 2)) (of_string int "+0" 0)
  (Ok (0, 2)) (of_string int "-0" 0)
 *)
(*$T int
  is_error (of_string int "-glop" 0)
  is_error (of_string int "+glop" 0)
 *)

(* Print the float with enough decimal places to be read back, while avoiding
 * adding many garbage digits at the end.
 * Also avoids a trailing decimal dot, which JSON format won't accept.
 * Taken from OCaml toplevel code (typing/oprint.ml, float_repr): *)
let my_string_of_float f =
  let s1 = Printf.sprintf "%.12g" f in
  if f = float_of_string s1 then s1 else
  let s2 = Printf.sprintf "%.15g" f in
  if f = float_of_string s2 then s2 else
  Printf.sprintf "%.18g" f

(* General format: [sign] digits ["." [FFF]] [e [sign] EEE] *)
type float_part = IntStart | Int | Frac | ExpStart | Exp
let float nan inf minf : float t =
  fun () ->
  { printer = (fun o v ->
      match classify_float v with
      | FP_infinite -> if v < 0. then o minf else o inf
      | FP_nan -> o nan
      | _ -> o (my_string_of_float v)) ;
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
        | _ -> oo, s, n, sc, es, exp
      in
      if stream_starts_with i o nan then
        Ok (Stdlib.nan, o + String.length nan)
      else if stream_starts_with i o inf then
        Ok (infinity, o + String.length inf)
      else if stream_starts_with i o minf then
        Ok (neg_infinity, o + String.length minf)
      else (
        let oo, s, n, sc, es, exp = loop o o 1 0 0 1 0 IntStart in
        if oo > o then Ok (
            float_of_int (s * n) *. 10. ** float_of_int (es * exp - sc), oo)
        else parse_error o "Expected floating number"
      )) ;
    descr = fun _ -> "float" }
(*$= float & ~printer:id
  "-0.00010348413604" (to_string (float "nan" "inf" "-inf") (-0.00010348413604))
  "nan" (to_string (float "nan" "inf" "-inf") nan)
  "inf" (to_string (float "nan" "inf" "-inf") infinity)
  "-inf" (to_string (float "nan" "inf" "-inf") neg_infinity)
 *)
(*$= float & ~printer:(function Error e -> string_of_error e | Ok (f,i) -> Printf.sprintf "(%f, %d)" f i)
  (Ok (3.14, 4)) (of_string (float "nan" "inf" "-inf") "3.14" 0)
  (Ok (3.14, 6)) (of_string (float "nan" "inf" "-inf") "314e-2" 0)
  (Ok (3.14, 8)) (of_string (float "nan" "inf" "-inf") "0.0314E2" 0)
  (Ok (~-.3.14, 9)) (of_string (float "nan" "inf" "-inf") "-0.0314E2" 0)
  (Ok (42., 2)) (of_string (float "nan" "inf" "-inf") "42" 0)
  (Ok (~-.42., 3)) (of_string (float "nan" "inf" "-inf") "-42" 0)
  (Ok (42., 3)) (of_string (float "nan" "inf" "-inf") "42." 0)
  (Ok (~-.42., 4)) (of_string (float "nan" "inf" "-inf") "-42." 0)
  (Ok (42., 5)) (of_string (float "nan" "inf" "-inf") "+42e0" 0)
  (Ok (42., 6)) (of_string (float "nan" "inf" "-inf") "+42.e0" 0)
  (Ok (42., 7)) (of_string (float "nan" "inf" "-inf") "+42.0e0" 0)
  (Ok (1., 1)) (of_string (float "nan" "inf" "-inf") "1e" 0)
  (Ok (-0.00010348413604, 17)) \
    (of_string (float "nan" "inf" "-inf") "-0.00010348413604" 0)
  (Ok (infinity, 3)) (of_string (float "nan" "inf" "-inf") "inf" 0)
  (Ok (neg_infinity, 4)) (of_string (float "nan" "inf" "-inf") "-inf" 0)
 *)
(*$T float
  is_error (of_string (float "nan" "inf" "-inf") "glop" 0)
  is_error (of_string (float "nan" "inf" "-inf") "+glop" 0)
  is_error (of_string (float "nan" "inf" "-inf") "-glop" 0)
 *)

let option ?placeholder ppp =
  fun () ->
  let ppp_ = ppp ()
  and placeholder_ =
    match placeholder with
    | None -> None
    | Some ppp -> Some (ppp ()) in
  { printer = (fun o -> function None -> (match placeholder_ with
                                          | None -> ()
                                          | Some p_ -> p_.printer o ())
                               | Some x -> ppp_.printer o x) ;
    scanner = (fun i o ->
      match ppp_.scanner i o with
      | Ok (x, o') -> Ok (Some x, o')
      | Error _ as err ->
        (match placeholder_ with
        | None -> Ok (None, o)
        | Some p_ ->
          (match p_.scanner i o with
          | Ok (_, o') -> Ok (None, o')
          | Error _ -> err))) ;
    descr =
      fun depth ->
      match placeholder_ with
      | None -> "optional "^ ppp_.descr (depth + 1)
      | Some p_ -> ppp_.descr (depth + 1) ^" or "^ p_.descr (depth + 1) }
(*$= option & ~printer:(function Error e -> string_of_error e | Ok (None, _) -> Printf.sprintf "none" | Ok (Some d, o) -> Printf.sprintf "(%d, %d)" d o)
  (Ok (Some 42,4)) (of_string (option (cst "{" -+ int +- cst "}")) "{42}" 0)
  (Ok (None,0))    (of_string (option (cst "{" -+ int +- cst "}")) "pas glop" 0)
  (Ok (Some 42,4)) (of_string (option ~placeholder:(cst "nope") (cst "{" -+ int +- cst "}")) "{42}" 0)
  (Ok (None,4))    (of_string (option ~placeholder:(cst "nope") (cst "{" -+ int +- cst "}")) "nope" 0)
 *)
(*$T option
  is_error (of_string (option ~placeholder:(cst "nope") (cst "{" -+ int +- cst "}")) "pas glop" 0)
 *)

let default v ppp =
  fun () ->
  let ppp_ = ppp () in
  { printer = ppp_.printer ;
    scanner = (fun i o ->
      match ppp_.scanner i o with
        | Error _ -> Ok (v, o)
        | x -> x) ;
    descr = ppp_.descr }
(*$= default & ~printer:(function Error e -> string_of_error e | Ok (d, o) -> Printf.sprintf "(%d, %d)" d o)
  (Ok (42,4)) (of_string (default 17 (cst "{" -+ int +- cst "}")) "{42}" 0)
  (Ok (17,0)) (of_string (default 17 (cst "{" -+ int +- cst "}")) "pas glop" 0)
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

let list_print ep oc lst =
  Printf.fprintf oc "[" ;
  List.iteri (fun i e ->
    Printf.fprintf oc "%s%a"
      (if i > 0 then "; " else "")
      ep e) lst ;
  Printf.fprintf oc "]"
let string_print oc str = Printf.fprintf oc "%S" str
let string_pair_print oc (s1, s2) = Printf.fprintf oc "(%S,%S)" s1 s2

let possible_word_boundary ?prev i o =
  let is_char c =
    assert (String.length c = 1) ;
    let c = c.[0] in
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
    c = '_'
  in
  match prev with
  | None -> true
  | Some p -> not (is_char (i o 1) && is_char p)

(* To be able to "reorder" records fields we need a function able to tell us
 * the length of a value, without knowing its type. With it we can read a
 * sequence of identifier * anything, and deal with it.
 * Delims are used to know where the value ends. *)
let rec skip_any groupings delims ?prev i o =
  (* Here we only deal with a value up to next delimiter (or closing grouping),
   * but if we encounter a group opening we switch to skip_group which will
   * (recursively) skips as many values to reach the end of the group. *)
  let o = skip_blanks i o in
  let c = i o 1 in
  if c = "" then Some o else
  if c = "\"" then (
    trace "skip_any: found a string at %d, skipping it." o ;
    skip_string i (o+1)) else
  if possible_word_boundary ?prev i o then
    if List.exists (stream_starts_with i o) delims then (
      trace "skip_any: found a delimiter at %d: %S"
        o (List.find (stream_starts_with i o) delims) ;
      Some o) else
    if List.exists (fun (_, cls) -> stream_starts_with i o cls) groupings then (
      (* Ok but this value can be itself in an outside group so group ends
       * must count as delimiters: *)
      trace "skip_any: found the end of an enclosing group at %d: %S"
        o (snd (List.find (fun (_, cls) -> stream_starts_with i o cls) groupings)) ;
      Some o) else
    match List.find (fun (opn, _) -> stream_starts_with i o opn) groupings with
    | exception Not_found ->
      (* Not a group: this char is part of the value that we skip *)
      (skip_any [@tailcall]) groupings delims ~prev:c i (o+1)
    | opn, cls ->
      trace "skip_any: found a %S,%S group starting at %d" opn cls o ;
      skip_group cls groupings delims i (o + String.length opn)
  else
    (* Not a group: this char is part of the value that we skip *)
    (skip_any [@tailcall]) groupings delims ~prev:c i (o+1)

(* Like skip_any but skip a sequence of values, as far as closing the opened group: *)
(* FIXME: skip_any called in non-tail position *)
and skip_group cls groupings delims i o =
  (* reads as many values and delimiters as to get out of that group. *)
  let clsl = String.length cls in
  let o = skip_blanks i o in
  if stream_starts_with i o cls then (
    trace "skip_group: group start with close %S, ending" cls ;
    Some (o + clsl)
  ) else (
    trace "skip_group: recursing into skip_any from pos %d" o ;
    match skip_any groupings delims i o with
    | None -> None
    | Some o ->
      trace "skip_group: skip_any returned at pos %d" o ;
      let o = skip_blanks i o in
      if stream_starts_with i o cls then (
        trace "skip_group: that's the end of this group, continue as %d." (o + clsl) ;
        Some (o + clsl)
      ) else (
        trace "skip_group: not the end of this group, we should have a delim (%a)."
          (list_print string_print) delims ;
        match List.find (stream_starts_with i o) delims with
        | exception Not_found ->
          trace "...yet this is not a delimiter?!" ;
          None
        | delim -> (* If we found a delim then we are not done yet *)
          let o = o + String.length delim in
          trace "skip_any: found a delimiter ending at %d" o ;
          skip_group cls groupings delims i o
      )
  )
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

let opened_group groupings i o =
  let o = skip_blanks i o in
  (* Look for a group opening at position o *)
  match List.find (fun (opn, _) -> stream_starts_with i o opn) groupings with
  | exception Not_found -> None
  | opn, _ as group -> Some (group, (o + String.length opn))

(* Swallow closing groups (previously opened) *)
let skip_closed_groups opened i o =
  let rec loop o = function
    | [] -> Some o
    | (_, cls)::rest ->
      let o = skip_blanks i o in
        let len = String.length cls in
        if i o len <> cls then None else loop (o + len) rest
  in
  loop o opened

(* Instead of a normal ppp, the printers used by unions take
 * an optional value and prints only when it's set - with the idea
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
  (string t (* name ppp *) -> bool (* do we need a separator? *) ->
    (writer -> 'a -> bool) (* like an 'a printer but returns if there was any output *)) *
  (string -> 'a scanner) *  (* string is: field value that's going to be read *)
  (int -> string) (* how to build the type name *)

let union opn cls eq groupings delims name_ppp (p, s, descr : 'a u) : 'a t =
  fun () ->
  let name_ppp_ = name_ppp () in
  { printer = (fun o x ->
      o opn ;
      p name_ppp false o x |> ignore ;
      o cls) ;
    scanner = (fun i union_start ->
      let opn_len = String.length opn
      and cls_len = String.length cls in
      (* There could be some grouping, skip it (unless that's our opening). *)
      let o = skip_blanks i union_start in
      match i o opn_len with
      | str when str = opn ->
        trace "union: found union opn %S" opn ;
        let o = o + opn_len in
        let name_start = skip_blanks i o in
        (match name_ppp_.scanner i name_start with
        | Error _ as e -> e
        | Ok (name, o') ->
          trace "union: found union name %S at %d" name name_start ;
          (* Now skip the eq sign *)
          let o' = skip_blanks i o' in
          let eq_len = String.length eq in
          (match i o' eq_len with
          | e when e = eq ->
            trace "union: found eq %S" eq ;
            let value_start = skip_blanks i (o' + eq_len) in
            (* Now the value *)
            let delims' = if cls_len > 0 then cls::delims else delims in
            (match skip_any groupings delims' i value_start with
            | None -> parse_error value_start (Printf.sprintf "Cannot delimit value for %S" name)
            | Some value_stop ->
              trace "union: found value, up to o=%d (starting at %d)" value_stop value_start ;
              let cls_pos = skip_blanks i value_stop in
              (* Check we have cls: *)
              (match i cls_pos cls_len with
              | str when str = cls ->
                trace "union: found cls %S at %d" cls cls_pos ;
                let chop_str = i value_start (cls_pos - value_start) in
                let skipped_left, value =
                  chop_sub chop_str 0 (cls_pos - value_start) in
                (* Keep value start up to date as we will need it to fix error
                 * location: *)
                let value_start = value_start + skipped_left in
                trace "union: found value %S" value ;
                (* Here we have an issue. We may fail to parse the value in
                 * case of extraneous groupings again. This is a bit annoying
                 * since some parsers may depends on the groups to be present
                 * (tuples...) so we cannot remove groups preemptively. We
                 * have to try to parse again while removing the grouping
                 * layers one by one. So for instance if we are given
                 * (((1,2))) as a int pair, we must try first with 3, then 2
                 * then 1 parentheses which will eventually succeed (notice 0
                 * parentheses would fail). *)
                let rec try_ungroup best_err opened i o =
                  trace "union: trying to parse value for label %S" name ;
                  match s name i o with
                  | Ok (v, o) ->
                    trace "union: parsed value!" ;
                    (* If we had to open some groups, check we can now close
                     * them: *)
                    (match skip_closed_groups opened i o with
                    | None ->
                      trace "union: Cannot close %d opened groups around value %S" (List.length opened) value ;
                      parse_error o "Expected closing group"
                    | Some oo -> Ok (v, oo))
                  (* If we receive an UnknownField error then there is no point
                   * ungrouping further. Instead we want to report this error, with
                   * the record label itself as the error: *)
                  | Error (_, UnknownField (n, _)) ->
                    trace "union: subvalue failed with unknownfield %S at %d" n o ;
                    Error (best_error best_err
                                      (name_start, UnknownField (n, value_stop)))
                  (* In other error cases try to ungroup: *)
                  | Error r ->
                    let best_err = best_error best_err r in
                    trace "union: cannot parse value from %d (%s) (best error so far: %s)"
                      o (string_of_error r) (string_of_error best_err) ;
                    (match opened_group (* FIXME: another type of groups here, with just "(",")" *) groupings i o with
                    | None ->
                      trace "union: but this is not a group opening. Game over!" ;
                      Error best_err
                    | Some (group, o) ->
                      let o = skip_blanks i o in
                      trace "union: retry from %d after the opened group" o ;
                      try_ungroup best_err (group::opened) i o)
                in
                let ii = string_reader ~offset:value_start value in
                let best_err = -1, NotDone "no error" in
                (match try_ungroup best_err [] ii value_start with
                | Error _ as e -> e
                | Ok (v, oo) ->
                  let oo = skip_blanks i oo in
                  (* We should have read everything *)
                  if oo = cls_pos then
                    Ok (v, cls_pos + cls_len)
                  else (
                    trace "union: garbage at end of value %S from offset %d" value oo ;
                    parse_error oo
                      (Printf.sprintf "Garbage at end of value for %S" name)))
              | _ -> parse_error cls_pos (Printf.sprintf "Expected closing of union %S" cls)))
          | _ -> parse_error o' (Printf.sprintf "Expected separator %S" eq)))
      | _ -> parse_error o (Printf.sprintf "Expected opening of union %S" opn)) ;
    descr = fun depth -> descr (depth + 1) }

(* Combine two ppp into a pair of options. *)
let alternative var_sep (p1, s1, id1 : 'a u) (p2, s2, id2 : 'b u) : ('a option * 'b option) u =
  (fun name_ppp need_sep o (v1, v2) ->
    let output need_sep p o = function
      | None -> need_sep
      | Some v ->
          let had_output = p name_ppp need_sep o v in
          need_sep || had_output in
    let need_sep = output need_sep p1 o v1 in
    output need_sep p2 o v2),
  (fun n i o ->
    match s1 n i o with
    | Ok (x, o) -> Ok ((Some x, None), o)
    | Error (_, UnknownField _) ->
      (match s2 n i o with
      | Ok (x, o) -> Ok ((None, Some x), o)
      | Error _ as err -> err)
    | Error _ as err -> err),
  (fun depth ->
     if depth > 12 then "..."
     else id1 (depth+1) ^ var_sep ^ id2 (depth+1))

let variant eq sep id_sep ?default name (ppp : 'a t) : 'a u =
  (fun name_ppp need_sep o v ->
    if default = Some v then false else (
      if need_sep then o sep ;
      let name_ppp_ = name_ppp () and ppp_ = ppp () in
      name_ppp_.printer o name ;
      o eq ;
      ppp_.printer o v ;
      true)),
  (fun n i o ->
    if n <> name then unknown_field o o n
    else let ppp_ = ppp () in ppp_.scanner i o),
  (fun depth ->
     let ppp_ = ppp () in
     let d = ppp_.descr depth in
     if d = "" then name else name ^ id_sep ^ d)

(* Like unit but with no representation, useful for
 * constructor without arguments *)
let none : unit t =
  fun () ->
  { printer = (fun _o () -> ()) ;
    scanner = (fun _i o -> Ok ((), o)) ;
    descr = fun _ -> "" }
(*$= none
  (Ok ((), 0)) (of_string none "" 0)
 *)


(*$inject
  let groupings = [ "(",")" ; "[","]" ; "[|","|]" ]
  let delims = [ "," ; ";" ; "=>" ]
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
(*$= color & ~printer:(function Error e -> string_of_error e | Ok (p, o) -> Printf.sprintf "(%s, %d)" (to_string color p) o)
  (Ok (RGB (0,0,255), 12)) (of_string color "RGB(0,0,255)" 0)
  (Ok (RGB (0,0,255), 13)) (of_string color "RGB (0,0,255)" 0)
  (Ok (RGB (0,0,255), 15)) (of_string color "  RGB (0,0,255)" 0)
  (Ok (RGB (0,0,255), 22)) (of_string color "RGB  ( 0 ,  0, 255  ) " 0)
  (Ok (Transparent, 8)) (of_string color " Transp " 0)
 *)

(* When using union for building a record, aggregate the tree of pairs at
 * every field.  This merge has to be recursive so we must build the merge
 * function as we build the total tree of pairs with <->. *)
type 'a merge_u = 'a option -> 'a option -> 'a option

let record ?(extensible=false) opn cls eq sep groupings delims name_ppp ((p, s, descr : 'a u), (merge : 'a merge_u)) =
  fun () ->
  (* In a record we assume there are nothing special to open/close a single
   * field. If not, add 2 parameters in the record: *)
  let nf = union "" "" eq groupings (cls::delims) name_ppp (p, s, descr) in
  let nf_ = nf () in
  { printer = (fun o v ->
      o opn ;
      p name_ppp false o v |> ignore ;
      o cls) ;
    scanner = (fun i o ->
      let value_start = skip_blanks i o in
      let opn_len = String.length opn in
      let sep_len = String.length sep in
      let cls_len = String.length cls in
      match i value_start opn_len with
      | str when str = opn ->
        let o = skip_blanks i (value_start + opn_len) in
        let rec loop prev o =
          match nf_.scanner i o with
          | Error (_, r) as e ->
            (match extensible, r with
            | true, UnknownField (_, o2) ->
              (* If we got as far as parsing a name and isolating a value
               * then we can assume the structure is not finished yet. *)
              next_value prev o2
            | _ ->
              (* Maybe it's an error, but maybe we are just done. *)
              prev, o, e)
          | Ok (x, o) ->
            let prev' = merge prev (Some x) in
            next_value prev' o
        and next_value prev o =
          let o = skip_blanks i o in
          (match i o sep_len with
          | str when str = sep ->
            loop prev (o + sep_len)
          | _ -> prev, o, parse_error o "Expected a separator")
        in
        let res, o, e = loop None o in
        (* Check that we are done *)
        let o = skip_blanks i o in
        (match i o cls_len with
        | str when str = cls ->
          (match res with None -> parse_error o "Empty record"
                        | Some r -> Ok (r, o + cls_len))
        | _ -> e)
      | _ -> parse_error o (Printf.sprintf "Expected opening of record %S" opn)) ;
    descr = fun depth -> opn ^ descr (depth + 1) ^ cls }

(* But then we need a special combinator to also compute the merge of the pairs of pairs... *)
let sequence field_sep ((psi1 : 'a u), (m1 : 'a merge_u)) ((psi2 : 'b u), (m2 : 'b merge_u)) : (('a option * 'b option) u * ('a option * 'b option) merge_u) =
  (alternative field_sep psi1 psi2),
  (fun (v1 : ('a option * 'b option) option) (v2 : ('a option * 'b option) option) ->
    let map_get f = function
      | None -> None
      | Some x -> f x in
    (* In case both v1 and v2 are None we must still give m1 and m2 a chance to
     * set a default value before returning None for the whole pair: *)
    match
      m1 (map_get fst v1) (map_get fst v2),
      m2 (map_get snd v1) (map_get snd v2) with
    | None, None -> None
    | a, b -> Some (a, b))

let field eq sep id_sep ?default_in ?default_out name (ppp : 'a t) : ('a u * 'a merge_u) =
  (variant eq sep id_sep ?default:default_out name ppp),
  (fun r1 r2 ->
    if r2 = None then (
      if r1 = None then default_in else r1
    ) else r2)

(*$inject
  type person = { name: string ; age: int ; male: bool }
  let (<->) x y = sequence "; " x y

  let person : person t =
    record "{" "}" "=" ";" groupings delims identifier (
      field "=" "; " ": " "name" string <->
      field "=" "; " ": " "age" int <->
      field "=" "; " ": " ~default_in:true ~default_out:true "male" bool) >>:
      ((fun { name ; age ; male } -> Some (Some name, Some age), Some male),
       (function (Some (Some name, Some age), Some male) -> { name ; age ; male }
               | _ -> raise (MissingRequiredField "?")))
 *)
(*$= person & ~printer:id
  "{name=\"John\"; age=41}" (to_string person { name = "John"; age = 41; male = true })
 *)
(*$= person & ~printer:(function Error e -> string_of_error e | Ok (p, o) -> Printf.sprintf "(%s, %d)" (to_string person p) o)
  (Ok ({ name = "John"; age = 41; male = true }, 30)) \
    (of_string person "{name=\"John\";age=41;male=true}" 0)
  (Ok ({ name = "John"; age = 41; male = true }, 30)) \
    (of_string person "{age=41;name=\"John\";male=true}" 0)
  (Ok ({ name = "John"; age = 41; male = true }, 39)) \
    (of_string person " {  age=41 ; name = \"John\" ;male =true}" 0)
  (Ok ({name="John";age=41;male=true}, 28)) \
    (of_string person " { age = 41 ; name = \"John\"}" 0)
 *)

let to_unit def ppp =
  ppp >>: ((fun _ -> def), fun _ -> ())

let newline : 'a t =
  to_unit None (option (cst "\r")) -- cst "\n"

let hashtbl opn cls sep kv_sep pppk pppv =
  let fold f init h =
    Hashtbl.fold (fun k v i -> f i (k, v)) h init
  and of_rev_list l =
    let h = Hashtbl.create 11 in
    List.iter (fun (k, v) -> Hashtbl.add h k v) l ;
    h in
  let ppp = pppk +- kv_sep ++ pppv in
  seq "hashtbl" opn cls sep fold of_rev_list ppp

(*$inject
  type phonebook = (int, person) Hashtbl.t

  let phonebook : phonebook t = hashtbl "{" "}" ";" (cst ":") int person
 *)
(* Because Hashtbl.fold order is unspecified and can change from OCaml
 * major version to the next we'd rather be cautious about that one test: *)
(*$T phonebook
  let s = \
    let h = Hashtbl.create 2 in \
    Hashtbl.add h 111 { name = "Jena"; age = 41; male = false } ; \
    Hashtbl.add h 222 { name = "Bill"; age = 25; male = true } ; \
    to_string phonebook h in \
  s = "{111:{name=\"Jena\"; age=41; male=false};222:{name=\"Bill\"; age=25}}" || \
  s = "{222:{name=\"Bill\"; age=25};111:{name=\"Jena\"; age=41; male=false}}"
 *)

(* References: mostly ignore them *)
let ref ppp = ppp >>: ((!), ref)

(* The operations required by the PPX: *)
module Ops =
struct
  let (++) = (++)
  let (-+) = (-+)
  let (+-) = (+-)
  let (--) = (--)
  let (>>:) = (>>:)
  let cst = cst
  let char = char "'"
  let string = string
  let bool = bool
  let int = int
  let int8 = int8
  let uint8 = uint8
  let int16 = int16
  let uint16 = uint16
  let int24 = int24
  let uint24 = uint24
  let int32 = int32
  let uint32 = uint32
  let int40 = int40
  let uint40 = uint40
  let int48 = int48
  let uint48 = uint48
  let int56 = int56
  let uint56 = uint56
  let int64 = int64
  let uint64 = uint64
  let int128 = int128
  let uint128 = uint128
  let float = float "nan" "inf" "-inf"
  let list ppp = seq "list" "(" ")" ";" List.fold_left List.rev ppp
  let array ppp = seq "array" "(" ")" ";" Array.fold_left (fun l -> Array.of_list (List.rev l)) ppp
  let hashtbl pppk pppv = hashtbl "{" "}" ";" (cst "=>") pppk pppv
  (* TODO: set, map... *)
  let unit = cst "_"
  let none = none
  let option = option
  let ref = ref
end

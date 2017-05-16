open Stdint

exception NotImplemented
exception CannotBacktrackThatFar
exception IntegerOverflow
exception MissingRequiredField (* used by ppx *)

(* Some helpers: *)

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
  "" (chop_sub " " 1 1)
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
  try (
    for i = 0 to len-1 do
      if pref.[i] <> str.[i] then raise Exit
    done ;
    true
  ) with Exit -> false


module type IO_CONFIG =
sig
  type 'a ct
  val return : 'a -> 'a ct
  val bind : 'a ct -> ('a -> 'b ct) -> 'b ct

  (* files *)
  type input_chan
  type output_chan

  val stdin : input_chan
  val stdout : output_chan
  val stderr : output_chan
  val write_string : output_chan -> string -> unit ct
  (* Must return 0 (only) on EOF *)
  val read : input_chan -> bytes -> int -> int -> int ct
end

module type S =
sig
  type 'a ct
  type input_chan
  type output_chan

  type writer = string -> unit ct
  type reader = int -> int -> string ct
  type 'a printer = writer -> 'a -> unit ct
  type 'a scanner = reader -> int -> ('a * int) option ct
  type 'a t = { printer : 'a printer ;
                scanner : 'a scanner ;
                descr : string }
  (* Note that we want of/to_string to return/take direct values not promises *)
  val to_string : 'a t -> 'a -> string
  val to_out_channel : 'a t -> output_chan -> 'a -> unit ct
  val to_stdout : 'a t -> 'a -> unit ct
  val to_stderr : 'a t -> 'a -> unit ct

  val of_string : 'a t -> string -> int -> ('a * int) option
  val of_in_channel : ?buf_capacity:int -> 'a t -> input_chan -> int -> ('a * int) option ct
  val of_stdin : ?buf_capacity:int -> 'a t -> int -> ('a * int) option ct

  val identifier : string t
  val until : string -> reader -> int -> int option ct
  val until_any : string list -> reader -> int -> int option ct
  val next_word : (char -> bool) -> reader -> int -> (string * int) option ct
  val next_int : reader -> int -> (int * int) option ct
  val seq : string -> string -> string -> string -> ((int ct -> 'a -> int ct) -> int ct -> 'b -> 'c ct) -> ('a list -> 'b) -> 'a t -> 'b t

  module Ops : sig
    val (++) : 'a t -> 'b t -> ('a * 'b) t
    val (-+) : unit t -> 'a t -> 'a t
    val (+-) : 'a t -> unit t -> 'a t
    val (--) : unit t -> unit t -> unit t
    val (>>:) : 'a t -> (('b -> 'a) * ('a -> 'b)) -> 'b t
    val cst : string -> unit t
    val string : string t
    val bool : bool t
    val int : int t
    val int32 : int32 t
    val uint32 : uint32 t
    val int64 : int64 t
    val uint64 : uint64 t
    val int128 : int128 t
    val uint128 : uint128 t
    val float : float t
    val list : 'a t -> 'a list t
    val array : 'a t -> 'a array t
    val unit : unit t
    val none : unit t
    val option : ?placeholder:('b t) -> 'a t -> 'a option t
  end

  val default : 'a -> 'a t -> 'a t

  type 'a u =
    (string t -> bool -> 'a printer) *    (* name ppp, bool is: do we need a separator? *)
    (string -> 'a scanner) *  (* string is: field value that's going to be read *)
    string                    (* how to build the type name *)
  val union : string -> string -> string -> (string * string) list -> string list -> string t -> 'a u -> 'a t
  val alternative : string -> 'a u -> 'b u -> ('a option * 'b option) u
  val variant : string -> string -> string -> string -> 'a t -> 'a u

  type 'a merge_u = 'a option -> 'a option -> 'a option
  val record : string -> string -> string -> string -> (string * string) list -> string list -> string t -> ('a u * 'a merge_u) -> 'a t

  val sequence : string -> ('a u * 'a merge_u) -> ('b u * 'b merge_u) -> ('a option * 'b option) u * ('a option * 'b option) merge_u

  val field : string -> string -> string -> ?default:'a -> string -> 'a t -> 'a u * 'a merge_u

  val to_unit : 'a -> 'a t -> unit t
  val newline : unit t
end

module Make (Conf : IO_CONFIG) =
struct
  (*$inject
    module P = PPP_block.P *)
  (*$< P *)

  include Conf

  (* First some helpers to help deal with the monadic style *)

  let if_ cond then_ else_ =
    bind cond (function
      | true -> then_
      | false -> else_)

  let may f = function None -> return () | Some x -> f x

  let stream_starts_with i o s =
    let l = String.length s in
    bind (i o l)
         (fun str -> return (str = s))

  let rec list_find f = function
    | [] -> return None
    | x::rest ->
      if_ (f x)
        (return (Some x))
        (list_find f rest)

  let list_exists f lst =
    bind (list_find f lst)
         (function Some _ -> return true | None -> return false)

  type writer = string -> unit ct
  type reader = int -> int -> string ct
  type 'a printer = writer -> 'a -> unit ct
  type 'a scanner = reader -> int -> ('a * int) option ct
  type 'a t = { printer : 'a printer ;
                scanner : 'a scanner ;
                descr : string }

  let to_string ppp v =
    let buf = Buffer.create 100 in
    let o str =
      Buffer.add_string buf str ;
      return () in
    bind (ppp.printer o v)
         (fun () -> return (Buffer.contents buf))

  let to_out_channel ppp chan v = ppp.printer (write_string chan) v
  let to_stdout pp v = to_out_channel pp stdout v
  let to_stderr pp v = to_out_channel pp stderr v

  let string_reader s o l =
    if o >= String.length s then return "" else
    let len =
      if o + l > String.length s then String.length s - o else l in
    return (String.sub s o len)

  let of_string ppp s =
    ppp.scanner (string_reader s)

  (* We want to allow non-seekable channels therefore we must keep a short
   * past-buffer in case the scanners wants to rollback a bit: *)
  let of_in_channel ?(buf_capacity=4096) ppp ic =
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
        bind (read ic buf read_into to_read)
          (fun read_len ->
             buf_length := !buf_length + read_len ;
             (* Printf.eprintf "Buffer: length=%d, offset=%d, content=%S\n%!"
                 !buf_length !buf_offset (Bytes.to_string buf) ; *)
             (* We are allowed to return a smaller string only in case of EOF: *)
             let str_start = o - !buf_offset in
             let str_len = min l (!buf_length - str_start) in
             if str_len < l && read_len > 0 then
               reader o l (* Keep reading *)
             else
               return (Bytes.sub_string buf str_start str_len))
      ) in
    ppp.scanner reader

  let of_stdin ?buf_capacity ppp = of_in_channel ?buf_capacity ppp stdin

  (* TODO: of_seekable_in_channel *)

  let next_eq w i o =
    if_ (stream_starts_with i o w)
      (return (true, o + String.length w))
      (return (false, o))

  let rec skip_blanks i o =
    bind (i o 1) (function
      | " " -> skip_blanks i (o + 1)
      | _ -> return o)

  (* C-like strings. Format: "..." *)
  type string_part = First | Char | BackslashStart | Backslash
  let string : string t =
    { printer = (fun o v -> o (Printf.sprintf "%S" v)) ;
      scanner = (fun i o ->
        let rec loop o l s bsn part =
          bind (i o 1) (fun chr ->
            match part, chr with
            | First, "\"" -> loop (o+1) l s bsn Char
            | Char, "\\" -> loop (o+1) l s bsn BackslashStart
            | Char, "\"" -> (* The only successful termination *)
              return (Some (string_of l s, o+1))
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
                if bsn > 255 then return None
                else loop (o+1) (Char.chr bsn :: l) (s+1) 0 Char
              ) else (
                (* 10+ so that we know when we have had 3 digits: *)
                loop (o+1) l s (10*bsn + digit_of d) Backslash
              )
            | _ -> return None) (* everything else is game-over *)
        in
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
          bind (i oo 1) (fun s ->
            if s = "" then return oo else
            let c = s.[0] in
            if is_letter c || c = '_' || oo > o && is_digit c then
              loop (oo+1)
            else
              return oo)
        in
        bind (loop o) (fun oo ->
          if oo > o then
            bind (i o (oo-o)) (fun s -> return (Some (s, oo)))
          else return None)) ;
      descr = "identifier" }
  (*$= identifier & ~printer:(function None -> "" | Some (i, o) -> Printf.sprintf "(%s,%d)" i o)
    (Some ("glop", 4)) (of_string identifier "glop" 0)
    (Some ("glop", 4)) (of_string identifier "glop\n" 0)
    (Some ("glop0", 5)) (of_string identifier "glop0" 0)
    None (of_string identifier "0glop" 0)
   *)

  let next_word_eq w i o =
    bind (next_eq w i o) (function
      | true, o ->
        bind (i o 1) (fun sep ->
          return ((sep = "" ||
                   let c = sep.[0] in
                   not (is_letter c || is_digit c || c = '_')), o))
      | x -> return x)

  let next_int i o =
    let rec loop n o =
      bind (i o 1) (fun s ->
        if str_is_digit s then (
          loop (n*10 + digit_of s) (o+1)
        ) else (
          return (n, o)
        ))
    in
    bind (loop 0 o) (fun (n, o')->
      if o' > o then return (Some (n, o'))
      else return None)
  (*$= next_int & ~printer:(function None -> "" | Some (n, o) -> Printf.sprintf "(%d, %d)" n o)
    (Some (42, 2)) (next_int (string_reader "42glop") 0)
   *)

  let rec until u i o =
    let l = String.length u in
    bind (i o l) (fun s ->
      if String.length s < l then return None else
      if s <> u then until u i (o + 1) else
      return (Some o))

  let until_any us i o =
    let mi, ma =
      List.fold_left (fun (mi, ma) u ->
        min mi (String.length u),
        max ma (String.length u)) (max_int, min_int) us in
    let rec loop o =
      bind (i o ma) (fun s ->
        if List.exists (fun u -> starts_with u s) us then
          return (Some o)
        else if String.length s < mi then
          return None
        else loop (o + 1))
    in
    loop o

  let next_word is_word i o =
    let rec skip_word o =
      bind (i o 1) (fun s ->
        if s = "" then return o else (
          let c = s.[0] in
          if is_word c then skip_word (o+1) else return o))
    in
    bind (skip_word o) (fun o' ->
      if o' = o then return None
      else bind (i o (o'-o)) (fun s -> return (Some (s, o'))))

  let seq name opn cls sep fold of_rev_list ppp =
    { printer = (fun o v ->
        bind (o opn) (fun () ->
          bind (
            fold (fun prev v' ->
              bind prev (fun i ->
                bind (if i > 0 then o sep else return ()) (fun () ->
                  bind (ppp.printer o v') (fun () ->
                    return (i + 1))))) (return 0) v) (fun _i ->
          o cls))) ;
      scanner = (fun i o ->
        let rec parse_sep prev o =
          if_ (stream_starts_with i o sep)
            (parse_item prev (o + String.length sep))
            (if_ (stream_starts_with i o cls)
              (return (Some (of_rev_list prev, o + String.length cls)))
              (return None))
        and parse_item prev o =
          bind (ppp.scanner i o) (function
            | Some (x, o') -> parse_sep (x::prev) o'
            | None ->
              if_ (stream_starts_with i o cls)
                (return (Some (of_rev_list prev, o + String.length cls)))
                (return None)) in
        if_ (stream_starts_with i o opn)
          (parse_item [] (o + String.length opn))
          (return None)) ;
      descr = name ^" of "^ ppp.descr }
  (*$= seq & ~printer:(function None -> "" | Some (l,o) -> Printf.sprintf "(%s, %d)" (String.concat ";" l) o)
    (Some (["a";"b";"cde"], 9)) \
      (of_string (seq "list" "[" "]" ";" List.fold_left List.rev identifier) "[a;b;cde]" 0)
    (Some (["a";"b";"cde"], 9)) \
      (of_string (seq "sequence" "" "" "--" List.fold_left List.rev identifier) "a--b--cde" 0)
   *)

  let (++) ppp1 ppp2 =
    { printer = (fun o (v1, v2) ->
        bind (ppp1.printer o v1) (fun () ->
          ppp2.printer o v2)) ;
      scanner = (fun i o ->
        bind (ppp1.scanner i o) (function
          | None -> return None
          | Some (v1, o) ->
            bind (ppp2.scanner i o) (function
              | None -> return None
              | Some (v2, o) -> return (Some ((v1, v2), o))))) ;
      descr = ppp1.descr ^ ppp2.descr }

  let (-+) ppp1 ppp2 =
    { printer = (fun o v2 ->
        bind (ppp1.printer o ()) (fun () ->
          ppp2.printer o v2)) ;
      scanner = (fun i o ->
        bind (ppp1.scanner i o) (function
          | None -> return None
          | Some ((), o) -> ppp2.scanner i o)) ;
      descr = ppp1.descr ^ ppp2.descr }

  let (+-) ppp1 ppp2 =
    { printer = (fun o v1 ->
        bind (ppp1.printer o v1) (fun () ->
          ppp2.printer o ())) ;
      scanner = (fun i o ->
        bind (ppp1.scanner i o) (function
          | None -> return None
          | Some (v, o) ->
            bind (ppp2.scanner i o) (function
              | None -> return None
              | Some (_, o) -> return (Some (v, o))))) ;
      descr = ppp1.descr ^ ppp2.descr }

  let (--) ppp1 ppp2 =
    { printer = (fun o _ ->
        bind (ppp1.printer o ()) (fun () ->
          ppp2.printer o ())) ;
      scanner = (fun i o ->
        bind (ppp1.scanner i o) (function
          | None -> return None
          | Some ((), o) ->
            bind (ppp2.scanner i o) (function
              | None -> return None
              | Some (_, o) -> return (Some ((), o))))) ;
      descr = ppp1.descr ^ ppp2.descr }

  let (>>:) ppp (f,f') =
    { printer = (fun o v -> ppp.printer o (f v)) ;
      scanner = (fun i o ->
        bind (ppp.scanner i o) (function
          | None -> return None
          | Some (x,o) -> return (Some (f' x, o)))) ;
      descr = ppp.descr }

  (* Always allow blanks around the constant *)
  let cst s =
    { printer = (fun o () -> o s) ;
      scanner = (fun i o ->
        bind (skip_blanks i o) (fun o ->
          let l = String.length s in
          if_ (stream_starts_with i o s)
            (bind (skip_blanks i (o + l))
                  (fun o -> return (Some ((), o))))
            (return None))) ;
      descr = s }

  let bool : bool t =
    { printer = (fun o v -> o (if v then "true" else "false")) ;
      scanner = (fun i o ->
        bind (next_word_eq "true" i o) (function
          | false, _ ->
            bind (next_word_eq "false" i o) (function
              | true, o -> return (Some (false, o))
              | _ -> return None)
          | x -> return (Some x))) ;
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
  type int_part = IntStart | Int
  let int128 : int128 t =
    { printer = (fun o v -> o (Int128.to_string v)) ;
      scanner = (fun i o ->
        let rec loop o oo s n part =
          bind (i o 1) (fun chr ->
            match part, chr with
            | IntStart, "+" -> loop (o+1) oo s n Int
            | IntStart, "-" -> loop (o+1) oo (~- s) n Int
            | (IntStart|Int), d when str_is_digit d ->
              let d = Int128.of_int (digit_of d) in
              loop (o+1) (o+1) s Int128.(add (mul n (of_int 10)) d) Int
            | _ -> return (oo, s, n))
        in
        bind (loop o o 1 Int128.zero IntStart) (fun (oo, s, n) ->
          let n = if s < 0 then Int128.neg n else n in
          if oo > o then return (Some (n, oo)) else return None)) ;
      descr = "integer" }

  let uint128 : uint128 t =
    { printer = (fun o v -> o (Uint128.to_string v)) ;
      scanner = (fun i o ->
        let rec loop o oo n part =
          bind (i o 1) (fun chr ->
            match part, chr with
            | IntStart, "+" -> loop (o+1) oo n Int
            | (IntStart|Int), d when str_is_digit d ->
              let d = Uint128.of_int (digit_of d) in
              loop (o+1) (o+1) Uint128.(add (mul n (of_int 10)) d) Int
            | _ -> return (oo, n))
        in
        bind (loop o o Uint128.zero IntStart) (fun (oo, n) ->
          if oo > o then return (Some (n, oo)) else return None)) ;
      descr = "integer" }

  let int64 : int64 t = int128 >>:
    ((fun n -> Int128.of_int64 n),
     (fun n -> Int128.to_int64 n))

  let uint64 : uint64 t = uint128 >>:
    ((fun n -> Uint128.of_uint64 n),
     (fun n -> Uint128.to_uint64 n))

  let int32 : int32 t = int128 >>:
    ((fun n -> Int128.of_int32 n),
     (fun n -> Int128.to_int32 n))

  let uint32 : uint32 t = uint128 >>:
    ((fun n -> Uint128.of_uint32 n),
     (fun n -> Uint128.to_uint32 n))

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
          bind (i o 1) (fun chr ->
            match part, chr with
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
            | _ -> return (oo, s, n, sc, es, exp))
        in
        bind (loop o o 1 0 0 1 0 IntStart) (fun (oo, s, n, sc, es, exp) ->
          if oo > o then return (Some (
            float_of_int (s * n) *. 10. ** float_of_int (es * exp - sc), oo))
          else return None)) ;
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

  let option ?placeholder ppp =
    { printer = (fun o -> function None -> return () | Some x -> ppp.printer o x) ;
      scanner = (fun i o ->
        bind (ppp.scanner i o) (function
          | Some (x, o') -> return (Some (Some x, o'))
          | None ->
            (match placeholder with
            | None -> return (Some (None, o))
            | Some p ->
              bind (p.scanner i o) (function
                | Some (_, o') -> return (Some (None, o'))
                | None -> return None)))) ;
      descr = match placeholder with
              | None -> "optional "^ ppp.descr
              | Some p -> ppp.descr ^" or "^ p.descr }
  (*$= option & ~printer:(function None -> "" | Some (None, _) -> Printf.sprintf "none" | Some (Some d, o) -> Printf.sprintf "(%d, %d)" d o)
    (Some (Some 42,4)) (of_string (option (cst "{" -+ int +- cst "}")) "{42}" 0)
    (Some (None,0))    (of_string (option (cst "{" -+ int +- cst "}")) "pas glop" 0)
    (Some (Some 42,4)) (of_string (option ~placeholder:(cst "nope") (cst "{" -+ int +- cst "}")) "{42}" 0)
    (Some (None,4))    (of_string (option ~placeholder:(cst "nope") (cst "{" -+ int +- cst "}")) "nope" 0)
    None               (of_string (option ~placeholder:(cst "nope") (cst "{" -+ int +- cst "}")) "pas glop" 0)
   *)

  let default v ppp =
    { printer = ppp.printer ;
      scanner = (fun i o ->
        bind (ppp.scanner i o) (function
          | None -> return (Some (v, o))
          | x -> return x)) ;
      descr = ppp.descr }
  (*$= default & ~printer:(function None -> "" | Some (d, o) -> Printf.sprintf "(%d, %d)" d o)
    (Some (42,4)) (of_string (default 17 (cst "{" -+ int +- cst "}")) "{42}" 0)
    (Some (17,0)) (of_string (default 17 (cst "{" -+ int +- cst "}")) "pas glop" 0)
   *)

  (* Generic record/union like facility *)

  (* Skip until end of string. Used by skip_any. *)
  let rec skip_string ?(backslashed=false) i o =
    bind (i o 1) (function
      | "\"" -> if backslashed then skip_string i (o+1) else return (Some (o+1))
      (* This is tab ; avoids confusing qtest: *)
      | "\x5c" -> skip_string ~backslashed:(not backslashed) i (o + 1)
      | "" -> return None
      | _ -> skip_string i (o+1))

  let debug = Printf.ifprintf (*Printf.fprintf*)

  (* To be able to "reorder" records fields we need a function able to tell us
   * the length of a value, without knowing its type. If we have this, then we
   * can read a sequence of identifier * anything, and deal with it.
   * Delims are used to know where the value ends. *)
  let rec skip_any groupings delims i o =
    (* Here we only deal with a value up to next delimiter (or closing grouping),
     * but if we encounter a group opening we switch to skip_group which will
     * (recursively) skips as many values to reach the end of the group. *)
    bind (skip_blanks i o) (fun o ->
      bind (i o 1) (fun c ->
        if c = "" then return (Some o)
        else if c = "\"" then (
          debug stderr "skip_any: found a string at %d, skipping it.\n%!" o ;
          skip_string i (o+1)
        ) else (
          if_ (list_exists (stream_starts_with i o) delims)
            (debug stderr "skip_any: found a delimiter at %d\n%!" o ;
             return (Some o))
            ((* Ok but this value can be itself in an outside group so group ends
              * must count as delimiters: *)
             if_ (list_exists (fun (_, cls) -> stream_starts_with i o cls) groupings)
               (debug stderr "skip_any: found the end of an enclosing group at %d\n%!" o ;
                return (Some o))
               (bind
                 (list_find (fun (opn, _) -> stream_starts_with i o opn) groupings)
                 (function
                  | None ->
                    (* Not a group: this char is part of the value that we skip *)
                    (* FIXME: this non-tail recursion is now much too slow *)
                    skip_any groupings delims i (o+1)
                  | Some (opn, cls) ->
                    debug stderr "skip_any: found a %S,%S group starting at %d\n%!" opn cls o ;
                    skip_group cls groupings delims i (o + String.length opn)))))))

  (* Like skip_any but skip a sequence of values, as far as closing the opened group: *)
  and skip_group cls groupings delims i o =
    (* reads as many values and delimiters as to get out of that group. *)
    let clsl = String.length cls in
    bind (skip_blanks i o) (fun o ->
      if_ (stream_starts_with i o cls)
        (return (Some (o + clsl)))
        (bind (skip_any groupings delims i o)
          (function
           | None -> return None
           | Some o ->
             bind (skip_blanks i o) (fun o ->
               if_ (stream_starts_with i o cls)
                (return (Some (o + clsl)))
                (bind (list_find (stream_starts_with i o) delims) (function
                  | None -> return None
                  | Some delim -> (* If we found a delim then we are not done yet *)
                    debug stderr "skip_any: found a delimiter at %d\n%!" o ;
                    let o = o + String.length delim in
                    skip_group cls groupings delims i o))))))
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
    bind (skip_blanks i o) (fun o ->
      (* Look for a group opening at position o *)
      bind (list_find (fun (opn, _) -> stream_starts_with i o opn) groupings)
        (function
          | None -> return None
          | Some (opn, _ as group) -> return (Some (group, (o + String.length opn)))))

  (* Swallow opened groups *)
  let skip_opened_groups groupings i o =
    let rec loop opened o =
      bind (skip_blanks i o) (fun o ->
        (* Look for a group opening at position o *)
        bind (opened_group groupings i o) (function
          | None -> return (opened, o)
          | Some (group, o) -> loop (group::opened) o))
    in
    loop [] o

  (* Swallow closing groups (previously opened) *)
  let skip_closed_groups opened i o =
    let rec loop o = function
      | [] -> return (Some o)
      | (_, cls)::rest ->
        bind (skip_blanks i o) (fun o ->
          let len = String.length cls in
          if_ (bind (i o len) (fun s -> return (s <> cls)))
            (return None)
            (loop (o + len) rest))
    in
    loop o opened

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
        bind (o opn) (fun () ->
          bind (p name_ppp false o x) (fun () ->
            o cls))) ;
      scanner = (fun i o ->
        let opn_len = String.length opn
        and cls_len = String.length cls in

        (* There could be some grouping, skip it. *)
        bind (skip_opened_groups groupings i o) (fun (opened, o) ->
          debug stderr "found %d opened groups\n%!" (List.length opened) ;
          bind (skip_blanks i o) (fun o ->
            bind (i o opn_len) (function
              | str when str = opn ->
                debug stderr "found union opn %S\n%!" opn ;
                let o = o + opn_len in
                bind (name_ppp.scanner i o) (function
                  | None -> return None
                  | Some (name, o') ->
                    debug stderr "found union name %S\n%!" name ;
                    (* Now skip the eq sign *)
                    bind (skip_blanks i o') (fun o' ->
                      let eq_len = String.length eq in
                      bind (i o' eq_len) (function
                        | e when e = eq ->
                          debug stderr "found eq %S\n%!" eq ;
                          bind (skip_blanks i (o' + eq_len)) (fun o' ->
                            (* Now the value *)
                            let delims' = if cls_len > 0 then cls::delims else delims in
                            bind (skip_any groupings delims' i o') (function
                              | None -> return None
                              | Some o ->
                                debug stderr "found value, up to o=%d (starting at %d)\n%!" o o' ;
                                (* If there was some opened grouping at the beginning, then we
                                 * must find them closed now: *)
                                bind (skip_closed_groups opened i o) (function
                                  | None -> return None
                                  | Some o ->
                                    debug stderr "found closed groupings\n%!" ;
                                    bind (skip_blanks i o) (fun cls_pos ->
                                      (* Check we have cls: *)
                                      bind (i cls_pos cls_len) (function
                                        | str when str = cls ->
                                          debug stderr "found cls %S\n%!" cls ;
                                          bind (i o' (cls_pos - o')) (fun chop_str ->
                                            let value = chop_sub chop_str 0 (cls_pos - o') in
                                            debug stderr "found value %S\n%!" value ;
                                            (* Here we have an issue. We may fail to parse the value in
                                             * case of extraneous groupings again. This is a bit annoying
                                             * since some parsers may depends on the groups to be present
                                             * (tuples...) so we cannot remove groups preemptively. We
                                             * have to try to parse again while removing the grouping
                                             * layers one by one. So for instance if we are given
                                             * (((1,2))) as a int pair, we must try first with 3, then 2
                                             * then 1 parentheses which will eventually succeed (notice 0
                                             * parentheses would fail). *)
                                            let rec try_ungroup opened i o =
                                              bind (s name i o) (function
                                                | Some (v, o) ->
                                                  debug stderr "parsed value!\n%!" ;
                                                  (* If we had to open some groups, check we can now close
                                                   * them: *)
                                                  bind (skip_closed_groups opened i o) (function
                                                    | None ->
                                                      debug stderr "Cannot close %d opened groups around value %S" (List.length opened) value ;
                                                      return None
                                                    | Some oo -> return (Some (v, oo)))
                                                | None ->
                                                  bind (opened_group groupings i o) (function
                                                    | None -> return None
                                                    | Some (group, o) ->
                                                      try_ungroup (group::opened) i o)) in
                                            let ii = string_reader value in
                                            bind (try_ungroup [] ii 0) (function
                                              | None -> return None
                                              | Some (v, oo) -> (* oo is the offset in the value only *)
                                                bind (skip_blanks ii oo) (fun oo ->
                                                  (* We should have read everything *)
                                                  if oo = String.length value then
                                                    return (Some (v, cls_pos + cls_len))
                                                  else (
                                                    debug stderr "garbage at end of value %S from offset %d\n%!" value oo ;
                                                    return None
                                                  ))))
                                        | _ -> return None)))))
                        | _ -> return None)))
              | _ -> return None)))) ;
      descr }

  (* Combine two ppp into a pair of options *)
  let alternative var_sep (p1, s1, id1 : 'a u) (p2, s2, id2 : 'b u) : ('a option * 'b option) u =
    (fun name_ppp need_sep o (v1,v2) ->
      bind (may (p1 name_ppp need_sep o) v1) (fun () ->
        let need_sep = need_sep || v1 <> None in
        may (p2 name_ppp need_sep o) v2)),
    (fun n i o ->
      bind (s1 n i o) (function
        | Some (x, o) -> return (Some ((Some x, None), o))
        | None ->
          bind (s2 n i o) (function
            | Some (x, o) -> return (Some ((None, Some x), o))
            | None -> return None))),
    (id1 ^ var_sep ^ id2)

  let variant eq sep id_sep name (ppp : 'a t) : 'a u =
    (fun name_ppp need_sep o v ->
      bind (if need_sep then o sep else return ()) (fun () ->
        bind (name_ppp.printer o name) (fun () ->
          bind (o eq) (fun () ->
            ppp.printer o v)))),
    (fun n i o ->
      if n <> name then return None
      else ppp.scanner i o),
    (if ppp.descr = "" then name else name ^ id_sep ^ ppp.descr)

  (* Like unit but with no representation, useful for
   * constructor without arguments *)
  let none : unit t =
    { printer = (fun _o () -> return ()) ;
      scanner = (fun _i o -> return (Some ((), o))) ;
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

  (* When using union for building a record, aggregate the tree of pairs at
   * every field.  This merge has to be recursive so we must build the merge
   * function as we build the total tree of pairs with <->. *)
  type 'a merge_u = 'a option -> 'a option -> 'a option
  let record opn cls eq sep groupings delims name_ppp ((p, s, descr : 'a u), (merge : 'a merge_u)) =
    (* In a record we assume there are nothing special to open/close a single
     * field. If not, add 2 parameters in the record: *)
    let nf = union "" "" eq groupings (cls::delims) name_ppp (p, s, descr) in
    { printer = (fun o v ->
        bind (o opn) (fun () ->
          bind (p name_ppp false o v) (fun () ->
            o cls))) ;
      scanner = (fun i o ->
        bind (skip_blanks i o) (fun o ->
          let opn_len = String.length opn in
          let sep_len = String.length sep in
          let cls_len = String.length cls in
          bind (i o opn_len) (function
            | str when str = opn ->
              bind (skip_blanks i (o + opn_len)) (fun o ->
                let rec loop prev o =
                  bind (nf.scanner i o) (function
                    | None -> return (prev, o)
                    | Some (x, o) ->
                      let prev' = merge prev (Some x) in
                      bind (skip_blanks i o) (fun o ->
                        bind (i o sep_len) (function
                          | str when str = sep ->
                            loop prev' (o + sep_len)
                          | _ -> return (prev', o)))) in
                bind (loop None o) (fun (res, o) ->
                  (* Check that we are done *)
                  bind (skip_blanks i o) (fun o ->
                    bind (i o cls_len) (function
                      | str when str = cls ->
                        (match res with None -> return None
                                      | Some r -> return (Some (r, o + cls_len)))
                      | _ -> return None))))
            | _ -> return None))) ;
      descr = opn ^ descr ^ cls }

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

  let to_unit def ppp =
    ppp >>: ((fun _ -> def), fun _ -> ())

  let newline =
    to_unit None (option (cst "\r")) -- cst "\n"

  (* The operations required by the PPX: *)
  module Ops =
  struct
    let (++) = (++)
    let (-+) = (-+)
    let (+-) = (+-)
    let (--) = (--)
    let (>>:) = (>>:)
    let cst = cst
    let string = string
    let bool = bool
    let int = int
    let int32 = int32
    let uint32 = uint32
    let int64 = int64
    let uint64 = uint64
    let int128 = int128
    let uint128 = uint128
    let float = float
    let list ppp = seq "list" "(" ")" ";" List.fold_left List.rev ppp
    let array ppp = seq "array" "(" ")" ";" Array.fold_left (fun l -> Array.of_list (List.rev l)) ppp
    let unit = cst "_"
    let none = none
    let option = option
  end

  (*$>*)
end

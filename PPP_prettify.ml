(* Take a string and return a nicer string *)

(*$inject let id x = x *)

let string_eq_at needle haystack idx =
  let rec loop i =
    if i >= String.length needle then true
    else idx + i < String.length haystack &&
         haystack.[idx + i] = needle.[i] &&
         loop (i + 1)
  in loop 0

(*$T string_eq_at
   string_eq_at "lop" "glop glop" 1
   string_eq_at "lop" "glop glop" 6
   string_eq_at "lop" "glop glop" 0 |> not
   string_eq_at "lop" "glop glop" 10 |> not
   string_eq_at "lop" "" 0 |> not
 *)

exception Return of string
let string_any_of_at needles haystack idx =
  try
    Array.iter (fun needle ->
      if string_eq_at needle haystack idx then raise (Return needle)
    ) needles ;
    raise Not_found
  with Return x -> x

exception Return2 of (string * string)
let string_any_of_pair_at needle_pairs haystack idx =
  try
    Array.iter (fun (start, _stop as pair) ->
      if string_eq_at start haystack idx then raise (Return2 pair)
    ) needle_pairs ;
    raise Not_found
  with Return2 x -> x

let last_linelen_of_buffer buf =
  let rec loop i =
    if i = 0 || Buffer.nth buf (i-1) = '\n' then i
    else loop (i-1) in
  let len = Buffer.length buf in
  len - loop len
(*$inject
  let buffer_of_string s =
    let b = Buffer.create (String.length s) in
    Buffer.add_string b s ;
    b
 *)

(*$= last_linelen_of_buffer & ~printer:string_of_int
  0 (last_linelen_of_buffer (Buffer.create 10))
  4 (last_linelen_of_buffer (buffer_of_string "glop"))
  0 (last_linelen_of_buffer (buffer_of_string "glop\n"))
  8 (last_linelen_of_buffer (buffer_of_string "glop\npas glop"))
 *)

let last_char_of_buffer buf =
  let len = Buffer.length buf in
  assert (len > 0) ;
  Buffer.nth buf (len - 1)

let is_in str c =
  try String.index str c |> ignore ;
      true
  with Not_found -> false

type chr = Chr of char | Verbatim of char
         | Open of string | Close of string | Token of string (* used by indenter *)
         | EOF

(* Takes a string, makes it a stream of Chr, give it to the function [f], and
 * convert back the output into a string: *)
let make_prettifier f str =
  let len = String.length str in
  let buf = Buffer.create (len * 2) in
  let k = function
    | Chr c | Verbatim c -> Buffer.add_char buf c
    | Open s | Close s | Token s -> Buffer.add_string buf s
    | EOF -> () in
  let f = f k in
  String.iter (fun c -> f (Chr c)) str ;
  f EOF ;
  Buffer.contents buf

type blank = Nope | Trim | NewLine | Space

(* Replace successive blank Chr by a single space or newline: *)
let compress_blanks ?(blanks=" \t") k =
  (* Remembers the most important blank encountered: *)
  let blank = ref Trim in
  (* Add b to the aggregated blanks: *)
  let compress b =
    match !blank with
    | Nope | Space -> blank := b (* promote to b *)
    | Trim | NewLine -> () in
  (* Output the aggregated blank: *)
  let flush_blank () =
    (match !blank with
    | Nope | Trim -> ()
    | NewLine -> k (Chr '\n')
    | Space -> k (Chr ' ')) ;
    blank := Nope
  in
  function
    | Chr '\n' -> compress NewLine
    | Chr c when is_in blanks c -> compress Space
    | Chr _ as x ->
        flush_blank () ;
        k x
    | Verbatim _ | Open _ | Close _ | Token _ as x ->
        k x
    | EOF as x ->
        blank := Trim ;
        k x

(*$= compress_blanks & ~printer:id
  "" (make_prettifier compress_blanks "")
  "glop" (make_prettifier compress_blanks "glop")
  "pas glop" (make_prettifier compress_blanks "pas glop")
  "pas glop" (make_prettifier compress_blanks "pas  glop")
  "pas glop" (make_prettifier compress_blanks "  pas  glop")
  "pas glop" (make_prettifier compress_blanks "pas  glop  ")
  "pas glop" (make_prettifier compress_blanks " pas  glop  ")
 *)

(* Turn the Chr that are surrounded by quotes into Verbatim characters: *)
let split_verbatim
      ?(quotes=[ '"','"' ; '\'','\'' ])
      ?(escape_char=Some '\\') (* Force to None to disable *)
      k =
  let end_verbatim = ref None in (* the quote sign that ends a verbatim *)
  let next_escaped = ref false in
  function
    | Chr c when !end_verbatim <> None && !next_escaped ->
        next_escaped := false ;
        k (Verbatim c)
    | Chr c when !end_verbatim = Some c ->
        end_verbatim := None ;
        k (Verbatim c)
    | Chr c when !end_verbatim <> None && escape_char = Some c ->
        assert (not !next_escaped) ;
        next_escaped := true ;
        k (Verbatim c)
    | Chr c when !end_verbatim <> None ->
        k (Verbatim c)
    | Chr c as x ->
        assert (!end_verbatim = None) ;
        (match List.assoc c quotes with
        | exception Not_found ->
            k x
        | q ->
            end_verbatim := Some q ;
            k (Verbatim c))
    | EOF as x -> k x
    | Verbatim _ | Open _ | Close _ | Token _ as x -> k x

(* Replace sequence of Chr that look like a open group, a close group, or a
 * token group, by actual Open, Close and Token items. *)
let tokenize
      ?(opens=["{|"; "[|"; "(|"; "<|"; "("; "{"; "["; "<"])
      ?(closes=["|}"; "|]"; "|)"; "|>"; ")"; "}"; "]"; ">"])
      ?(tokens=["=="; "=>"; "<="; "->"; "<-"; ":="; "=:"; "="])
      k =
  let last_chr = ref None in
  let group_of s =
    if List.mem s opens then (
      Open s
    ) else if List.mem s closes then (
      Close s
    ) else if List.mem s tokens then (
      Token s
    ) else raise Not_found
  in
  fun x ->
    match x, !last_chr with
    | Chr c, None ->
        last_chr := Some c
    | Chr c, Some last ->
        (* Try a full match first: *)
        let s = String.init 2 (function 0 -> last | _ -> c) in
        (match group_of s with
        | exception Not_found ->
            (* Then maybe last_char alone? *)
            let s = String.make 1 last in
            (match group_of s with
            | exception Not_found -> k (Chr last) ;
            | grp -> k grp) ;
            last_chr := Some c
        | grp ->
            k grp ;
            last_chr := None)
    | x, Some last ->
        let s = String.make 1 last in
        (match group_of s with
        | exception Not_found -> k (Chr last)
        | grp -> k grp) ;
        last_chr := None ;
        k x
    | x, None ->
        k x

(* Suppress lines made only of blanks (or empty): *)
let remove_empty_lines ?(blanks=" \t") k =
  let chars = ref [] in
  let has_content = ref false in
  let flush_line () =
    if !has_content then (
      List.rev !chars |>
      List.iter k ;
      has_content := false) ;
    chars := [] in
  fun x ->
    chars := x :: !chars ;
    match x with
    | Chr '\n' ->
        flush_line ()
    | Chr c ->
        if not !has_content && not (is_in blanks c) then
          has_content := true
    | Verbatim _ ->
        has_content := true
    | Open s | Close s | Token s ->
        if s <> "" then has_content := true
    | EOF ->
        let had_content = !has_content in
        flush_line () ;
        if not had_content then k x

(*$= remove_empty_lines & ~printer:id
  "glop" (make_prettifier remove_empty_lines "glop")
  "  glop  " (make_prettifier remove_empty_lines "  glop  ")
  "" (make_prettifier remove_empty_lines "  ")
  "glop" (make_prettifier remove_empty_lines " \n\nglop")
 *)

(* Look for pairs of matching Close/Open and indent them, leaving the Open at
 * the end of its line and making the corresponding Close at the beginning of
 * its. *)
let reindent ?(indent="\t") ?(blanks=" \t") k =
  (* The number of currently opened groups: *)
  let depth = ref 0 in
  (* Tells if we just added the indentation at the beginning of the line, so
   * that next blanks are ignored: *)
  let had_indent = ref true in
  (* Tells if we just added a space, so that next spaces are ignored: *)
  let had_space = ref false in
  (* Output a string as a sequence of Chr: *)
  let k_string = String.iter (fun c -> k (Chr c)) in
  (* For each item of the stack, output the indent string: *)
  let add_indent () =
    had_indent := true ;
    had_space := false ;
    for i = 1 to !depth do
      k_string indent
    done in
  let output x =
    if x = Chr ' ' then (
      if not !had_space then (
        k x ;
        had_space := true
      )
    ) else (
      k x ;
      had_space := false
    ) in
  fun x ->
    match x with
    | Close s ->
        if !depth = 0 then
          Printf.eprintf "Unbalanced open/close groups (close %S)\n" s
        else (
          output (Chr '\n') ;
          decr depth ;
          add_indent () ;
          had_indent := false ;
          output x ;
        )
    | Token _ -> (* Put some space around separators *)
        output (Chr ' ') ;
        output x ;
        output (Chr ' ')
    | Open _ ->
        incr depth ;
        (* Adds space before the open group: *)
        if not !had_indent then output (Chr ' ') ;
        output x ;
        output (Chr '\n') ;
        add_indent ()
    | Chr c ->
        if !had_indent && is_in blanks c then (
          (* swallow blanks after indent *)
        ) else (
          had_indent := false ;
          output x
        )
    | Verbatim _ | EOF ->
        had_indent := false ;
        output x

(* Suppress any trailing blanks: *)
let no_trailing_blanks ?(blanks=" \t") k =
  (* Accumulate all the successive blanks we've met but not output yet: *)
  let last_blanks = ref [] in
  (* Do output all those blanks: *)
  let flush () =
    List.rev !last_blanks |>
    List.iter k ;
    last_blanks := [] in
  function
    | (Chr c as x) when is_in blanks c ->
        last_blanks := x :: !last_blanks
    | Chr '\n' as x ->
        last_blanks := [] ;
        k x
    | x ->
        flush () ;
        k x

(* Limit the output lines to the given width, while preserving indentation, by
 * substituting newlines for some of the blanks or adding newlines after
 * separators: *)
let add_newlines ?(columns=80) ?(blanks=" \t") ?(separators=",;") k =
  let curcol = ref 0 in
  let line_indent = ref [] in
  (* Buffered items not yet output: *)
  let last_seq = ref [] in
  (* Length in characters of last_seq: *)
  let last_seq_len = ref 0 in
  let had_content = ref false in (* Before the current seq *)
  let write lst =
    List.rev lst |>
    List.iter (fun x -> k x ; incr curcol) in
  let flush_seq () =
    if !last_seq <> [] then (
      if !had_content && !curcol + !last_seq_len >= columns then (
        k (Chr '\n') ;
        curcol := 0 ;
        write !line_indent
      ) else (
        if !had_content then k (Chr ' ')
      ) ;
      write !last_seq ;
      last_seq := [] ;
      last_seq_len := 0 ;
      had_content := true
    ) in
  let append x len =
    last_seq := x :: !last_seq ;
    last_seq_len := !last_seq_len + len in
  function
    | Chr '\n' as x ->
        flush_seq () ;
        k x ;
        curcol := 0 ;
        line_indent := [] ;
        had_content := false
    | (Chr c as x) when is_in blanks c ->
        flush_seq () ;
        if not !had_content then (
          line_indent := x :: !line_indent ;
          incr curcol ;
          k x
        )
    | (Chr c as x) when is_in separators c ->
        append x 1 ;
        flush_seq ()
    | Chr _ | Verbatim _ as x ->
        append x 1
    | Open s | Close s | Token s as x ->
        append x (String.length s)
    | EOF as x ->
        flush_seq () ;
        k x

(* Fix improper output by not allowing the output to end without a newline: *)
let newline_at_end_of_file k =
  let last_chr = ref None in
  function
  | EOF ->
      (match !last_chr with
      | Some (Chr '\n') | Some (Verbatim '\n') -> ()
      | Some (Open s) | Some (Close s) | Some (Token s)
          when s <> "" && s.[String.length s - 1] = '\n' -> ()
      | _ ->
          k (Verbatim '\n')) ;
      k EOF
  | x ->
      last_chr := Some x ;
      k x

let prettifier
      ?blanks ?quotes ?escape_char
      ?indent ?columns ?separators
      ?opens ?closes ?tokens
      k =
  split_verbatim ?quotes ?escape_char (
    tokenize ?opens ?closes ?tokens (
      compress_blanks ?blanks (
        reindent ?indent (
          add_newlines ?columns ?blanks ?separators (
            remove_empty_lines ?blanks (
              no_trailing_blanks ?blanks (
                newline_at_end_of_file k)))))))

let prettify
      ?blanks ?quotes ?escape_char
      ?indent ?columns ?separators
      ?opens ?closes ?tokens =
  make_prettifier (
    prettifier
      ?blanks ?quotes ?escape_char ?indent ?columns ?separators
      ?opens ?closes ?tokens)

(*$= prettify & ~printer:id
  "\n" (prettify "")

  "\n" (prettify "\n")

  "{\n}\n" (prettify "{}")

  "{\n}\n" (prettify "{}\n")

  "{\n}\n" (prettify "\n{}\n")

  "{\n}\n" (prettify "\n{\n}\n")

  "glop\n" (prettify "glop")

  {|(\
	glop\
)\
|} (prettify "(glop)")

  {|(\
	glop [\
		"glop  (pas  glop)"\
	]\
)\
|} (prettify "(glop   [ \"glop  (pas  glop)\" ])")

  {|{\
	key => "val=>ue"\
}\
|} (prettify "{key=>\"val=>ue\"}")
 *)

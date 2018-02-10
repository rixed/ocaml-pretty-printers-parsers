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

type chr = Chr of char | Verbatim of char | EOF

let make_prettifier f str =
  let len = String.length str in
  let buf = Buffer.create (len * 2) in
  let k = function
    | Chr c | Verbatim c -> Buffer.add_char buf c
    | EOF -> () in
  let f = f k in
  String.iter (fun c -> f (Chr c)) str ;
  f EOF ;
  Buffer.contents buf

type blank = Nope | Trim | NewLine | Space

let compress_blanks ?(blanks=" \t") k =
  let blank = ref Trim in
  let compress b =
    match !blank with
    | Nope | Space -> b
    | Trim | NewLine -> !blank in
  let flush_blank () =
    match !blank with
    | Nope | Trim -> ()
    | NewLine -> k (Chr '\n')
    | Space -> k (Chr ' ') in
  function
    | Chr '\n' ->
        blank := compress NewLine
    | Chr c when is_in blanks c ->
        blank := compress Space
    | Chr _ as x ->
        flush_blank () ;
        blank := Nope ;
        k x
    | Verbatim _ as x ->
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

let split_verbatim
      ?(quotes=[ '"','"' ; '\'','\'' ])
      ?(escape_char=Some '\\') (* Force to None to disable *)
      k =
  let end_verbatim = ref None in
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
            k x)
    | EOF as x -> k x
    | Verbatim _ as x -> k x

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
        if not !has_content then
          has_content := true
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

let reindent ?(indent="\t")
             ?(blanks=" \t")
             ?(pars=[ '(',')' ; '{','}' ; '[',']' ; '<','>' ])
             ?(separators=":=") k =
  let stack = ref [] in
  let had_indent = ref true in
  let k_string = String.iter (fun c -> k (Chr c)) in
  let add_indent () =
    let rec loop = function
      | [] -> ()
      | _ :: rest ->
          k_string indent ;
          loop rest in
    had_indent := true ;
    loop !stack in
  fun x ->
    match x, !stack with
    | Chr c, cls::stk when cls = c ->
        k (Chr '\n') ;
        stack := stk ;
        add_indent () ;
        had_indent := false ;
        k x ;
    | Chr c, _ when is_in separators c ->
        k (Chr ' ') ;
        k x ;
        k (Chr ' ')
    | Chr c, _ ->
        (match List.assoc c pars with
        | exception Not_found ->
            if !had_indent && is_in blanks c then (
              (* swallow blanks after indent *)
            ) else (
              had_indent := false ;
              k x
            )
        | cls ->
            stack := cls :: !stack ;
            if not !had_indent then k (Chr ' ') ;
            k x ;
            k (Chr '\n') ;
            add_indent ())
    | (Verbatim _ | EOF), _ ->
        had_indent := false ;
        k x

let no_trainling_blanks ?(blanks=" \t") k =
  let last_blanks = ref [] in
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

let add_newlines ?(columns=80) ?(blanks=" \t") ?(separators=",;") k =
  let curcol = ref 0 in
  let line_indent = ref [] in
  let last_seq = ref [] in
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
  let append x =
    last_seq := x :: !last_seq ;
    incr last_seq_len in
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
        append x ;
        flush_seq ()
    | Chr _ | Verbatim _ as x ->
        append x
    | EOF as x ->
        flush_seq () ;
        k x

let remove_blanks ?(blanks=" \t") k = function
  | Chr c when is_in blanks c -> ()
  | x -> k x

let prettifier ?blanks ?quotes ?escape_char ?indent ?pars ?columns ?separators k =
  split_verbatim ?quotes ?escape_char (
    remove_blanks ?blanks (
      reindent ?indent ?pars (
        add_newlines ?columns ?blanks ?separators (
          remove_empty_lines ?blanks (
            no_trainling_blanks ?blanks k)))))

let prettify ?blanks ?quotes ?escape_char ?indent ?pars ?columns =
  make_prettifier (prettifier ?blanks ?quotes ?escape_char ?indent ?pars ?columns)

(*$= prettify & ~printer:id
  "glop" (prettify "glop")
  {|(\
	glop\
)|} (prettify "(glop)")
  {|(\
	glop [\
		"glop  (pas  glop)"\
	]\
)|} (prettify "(glop   [ \"glop  (pas  glop)\" ])")
 *)

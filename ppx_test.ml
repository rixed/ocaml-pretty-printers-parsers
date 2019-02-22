(* Test OCaml serialization *)

module A = struct
  type t1 = int [@@ppp PPP_OCaml]  (* <- build t1_ppp_ocaml for int *)
                [@@ppp PPP_JSON]   (* and t1_ppp_json *)
end

module B = struct
  type t2 = int * string [@@ppp PPP_OCaml]  (* <- build t2_ppp_ocaml for pair int string *)

  type t3 = { foo : int [@ppp_default 17] ;
              bar : bool ;
              recursive : t3 list [@ppp_default []] } [@@ppp PPP_OCaml]

  type t4 = Foo of int | Bar of string * int * bool | Baz [@@ppp PPP_OCaml] [@@ppp PPP_JSON]
end

type t5 = B.t4 option [@@ppp PPP_OCaml] [@@ppp PPP_JSON]

type t6 = PasGlop | Glop of { a:A.t1; b:t5 } [@@ppp PPP_OCaml] [@@ppp PPP_JSON]

type t7 = Zap of { a: t6 option } [@@ppp PPP_OCaml]

type t8 = int * string * bool * float [@@ppp PPP_OCaml]

open Stdint
type t9 = { u40 : uint40 ; i48 : int48 ; u56 : uint56 } [@@ppp PPP_OCaml]

type t10 = { foo10 : int ; bar10 : (string, int) Hashtbl.t } [@@ppp PPP_OCaml]

type t11 = int list ref [@@ppp PPP_OCaml]
type t12 = { f1 : t11 ref } [@@ppp PPP_OCaml]

type t13 = { addr : Unix.inet_addr } [@@ppp PPP_OCaml]

(* Recursive types: *)
type t14 = T1 of t15 | T2 of int [@@ppp PPP_OCaml]
and t15 = T3 of t14 | T4 of string [@@ppp PPP_OCaml]

(* Arrays: *)
type t16 = int array [@@ppp PPP_OCaml]

(* Deeper constructed types: *)
type t17 = Oye of t6 [@@ppp PPP_OCaml]
type t18 = OyeOye of t17 [@@ppp PPP_OCaml]
type t19 = (t18 * t17) array [@@ppp PPP_OCaml]
type t20 = { oyeoye : t18 ; oye : t17 [@ppp_default Oye PasGlop] } [@@ppp PPP_OCaml]

(* TODO: Parametric types *)
(*type 'a t16 = 'a list [@@ppp PPP_OCaml]*)

(* Now some JSON types *)

type j1 = { field1 : int [@ppp_default 15];
            field2 : string ;
            field3 : char [@ppp_default 'x'] ;
            field4 : int option ;
            field5 : bool option [@ppp_default Some false] } [@@ppp PPP_JSON]

type j2 = NoArg | Arg of int [@@ppp PPP_JSON]

type j3 = J31 | J32 of string [@@ppp PPP_JSON]

type j4 = { baz : j3 [@ppp_default J31] } [@@ppp PPP_JSON]

type j5 = int list [@@ppp PPP_JSON]

type j6_extens = { mandatory : int ; optional : string [@ppp_default "default"] } [@@ppp PPP_JSON] [@@ppp_extensible]

type j7_rename = { foo : int [@ppp_rename "bar"] ; bar : string [@ppp_rename "baz"] } [@@ppp PPP_JSON]

type j8 = { a : int ; b : float option } [@@ppp PPP_JSON]

type j9 = { foo : int ; hash : (int, float) Hashtbl.t } [@@ppp PPP_JSON]
type j10 = { hash : (string, bool) Hashtbl.t } [@@ppp PPP_JSON]
type j11 = PasJlop | Jlop of { c:int; d:j8 option } [@@ppp PPP_JSON]

type arrt = AU64 of uint64 array | AU32 of uint32 array | AFloat of float array [@@ppp PPP_JSON] [@@ppp PPP_OCaml]
type export_msg = { first : int ; columns : (string * bool * arrt) list } [@@ppp PPP_JSON] [@@ppp PPP_OCaml]

let () =
  let mouline ppp str =
    match PPP.of_string ppp str 0 with
    | Ok (x, _) -> PPP.to_string ppp x
    | Error e -> PPP.string_of_error e in
  let test_string_conv ppp s =
    let ppp_ = ppp () in
    Printf.printf "%s: %s\n" (ppp_.PPP.descr 0) (mouline ppp s) in
  test_string_conv t6_ppp_ocaml "Glop { a = 42 ; b = None }" ;
  test_string_conv t6_ppp_ocaml "Glop { a = 42 ; b = Some (Bar (\"bla\", 4, false)) }" ;
  test_string_conv t6_ppp_ocaml "Glop{a =42; b= Some (Bar(\"bla\"  , 4, true)) }" ;
  test_string_conv t6_ppp_ocaml "Glop{a=42;b=Some(Bar(\"bla\",4, true))}" ;
  test_string_conv t6_ppp_ocaml "Glop { a = 42 ; b = Some (Foo 42) }" ;
  test_string_conv t6_ppp_ocaml "Glop { a = 42 ; b = Some (Foo (42)) }" ;
  test_string_conv t6_ppp_ocaml "Glop { a = -2 ; b = Some Baz }" ;
  test_string_conv t6_ppp_ocaml "Glop { a = 0 ; b = Some(Baz) }" ;
  test_string_conv t6_ppp_json  "{\"Glop\": { \"a\": 42, \"b\": null } }" ;
  test_string_conv t7_ppp_ocaml "Zap { a = Some PasGlop }" ;
  test_string_conv t7_ppp_ocaml "Zap{ a= Some PasGlop}" ;
  test_string_conv t7_ppp_ocaml "Zap{a=Some(PasGlop)}" ;
  test_string_conv t7_ppp_ocaml "Zap{a=None}" ;
  test_string_conv B.t3_ppp_ocaml "{ foo=42; bar=false }" ;
  test_string_conv B.t3_ppp_ocaml "{ foo=42; bar =true; }" ;
  test_string_conv B.t3_ppp_ocaml "{ bar =true; recursive=[ {foo=1; bar=true; recursive=[]} ] }" ;
  test_string_conv t8_ppp_ocaml "(1, \"2\", true, 4.)" ;
  test_string_conv t8_ppp_ocaml "(1, \" escaped:\\\" \", true, 0)" ;
  test_string_conv t9_ppp_ocaml "{u40 = 42190; u56=429000 ; i48 = -42 }" ;
  test_string_conv t10_ppp_ocaml "{foo10 = 4; bar10= { \"glop\"=>42 ; \"pas\"=>1}}" ;
  test_string_conv t11_ppp_ocaml "[1; 2; 3]" ;
  test_string_conv t12_ppp_ocaml "{ f1 = [1;2;3] }" ;
  test_string_conv t13_ppp_ocaml "{ addr = \"192.168.0.42\" }" ;
  test_string_conv t14_ppp_ocaml "T1 (T3 (T2 42))" ;
  test_string_conv t16_ppp_ocaml "[| 1; 2; 3 |]" ;
  test_string_conv t19_ppp_ocaml "[|(OyeOye Oye PasGlop, Oye PasGlop)|]" ;
  test_string_conv t20_ppp_ocaml "{ oyeoye = OyeOye Oye PasGlop; oye = Oye PasGlop }" ;
  test_string_conv export_msg_ppp_ocaml "{ columns = [ (\"h1\",false, AU64 [|1|] ) ]; first = 1 }" ;
  test_string_conv export_msg_ppp_ocaml "{ first=1; columns= [ (\"h1\",false,AU64 [|1|]) ]}" ;
  test_string_conv j1_ppp_json "{\"field1\": 42, \"field2\": \"bla\", \"field4\": 10}" ;
  test_string_conv j1_ppp_json "{\"field1\": 42, \"field2\": \"bla\", \"field3\": \"z\", \"field4\": null}" ;
  test_string_conv j1_ppp_json "{\"field1\": 42, \"field2\": \"bla\", \"field4\": 1, \"field5\": true}" ;
  test_string_conv j1_ppp_json "{\"field2\": \" escaped:\\\" \", \"field4\": 1}" ;
  test_string_conv j2_ppp_json "{\"NoArg\":null}" ;
  test_string_conv j2_ppp_json "{\"Arg\":42}" ;
  test_string_conv j4_ppp_json "{ \"baz\": { \"J31\": null } }" ;
  test_string_conv j5_ppp_json "[42,12]" ;
  test_string_conv j5_ppp_json "[42, 12]" ;
  test_string_conv j5_ppp_json "[42 ,12]" ;
  test_string_conv j5_ppp_json "[ 42, 12 ] " ;
  test_string_conv j5_ppp_json " [42 , 12]" ;
  test_string_conv j5_ppp_json "  [ 42 , 12   ] " ;
  (* Check it's OK to have more fields than declared: *)
  test_string_conv j6_extens_ppp_json "{\"mandatory\":1, \"extra\": [1,2,3]}" ;
  test_string_conv j6_extens_ppp_json "{\"optional\" : \"present\" , \"extra\" : { \"foo\":42 }, \"mandatory\":1 }" ;
  (* Check field renaming *)
  test_string_conv j7_rename_ppp_json "{ \"bar\":42, \"baz\":\"glop\" }" ;
  test_string_conv j8_ppp_json "{ \"a\":42, \"b\":1 }" ;
  test_string_conv j8_ppp_json "{ \"a\":42, \"b\": null }" ;
  test_string_conv j8_ppp_json "{ \"a\":42, \"b\": \"nan\" }" ;
  test_string_conv j8_ppp_json "{ \"a\":42, \"b\": \"inf\" }" ;
  test_string_conv j8_ppp_json "{ \"a\":42, \"b\": \"-inf\" }" ;
  let r = PPP.of_string_exc j8_ppp_json "{ \"a\":42, \"b\": null }" in
  Printf.printf "j8 null -> %s\n" (match r.b with None -> "none" | Some f -> string_of_float f) ;
  let r = PPP.of_string_exc j8_ppp_json "{ \"a\":42, \"b\": \"nan\" }" in
  Printf.printf "j8 null -> %s\n" (match r.b with None -> "none" | Some f -> string_of_float f) ;
  let r = PPP.of_string_exc j8_ppp_json "{ \"a\":42, \"b\": \"-inf\" }" in
  Printf.printf "j8 null -> %s\n" (match r.b with None -> "none" | Some f -> string_of_float f) ;
  test_string_conv j9_ppp_json "{ \"foo\":42, \"hash\": { \"34\": \"inf\", \"42\": 42.0 } }" ;
  test_string_conv j10_ppp_json "{\"hash\":{\"foo\": true, \"bar\":false}}" ;
  test_string_conv j11_ppp_json "{\"Jlop\": { \"c\": 1, \"d\": null }}" ;
  test_string_conv arrt_ppp_json "{ \"AFloat\" : [ 26.3129910322, 93.4604360475 ] }" ;
  test_string_conv arrt_ppp_json "{ \"AU64\" : [ 1493409971653419, 1493409400273526 ] }" ;
  test_string_conv export_msg_ppp_json "{\"columns\":[[\"h1\",false,{\"AU64\":[1]}]],\"first\":1}" ;
  test_string_conv export_msg_ppp_json "{\"first\":1,\"columns\":[[\"h1\",false,{\"AU64\":[1]}]]}" ;
  let str = "{\n\
     \"columns\" : [\n\
      [\n\
         \"h1\",\n\
         false,\n\
         { \"AU64\" : [ 1493409900015801, 1493409342783959 ] }\n\
      ], [\n\
         \"h2\",\n\
         false,\n\
         { \"AU64\" : [ 1493409971653419, 1493409400273526 ] }\n\
      ], [\n\
         \"h3\",\n\
         false,\n\
         { \"AFloat\" : [ 26.3129910322, 93.4604360475 ] }\n\
      ], [\n\
         \"h4\",\n\
         false,\n\
         { \"AFloat\" : [ 1949.54835042, 225383.363907 ] }\n\
      ], [\n\
         \"h5\",\n\
         false,\n\
         { \"AU32\" : [ 50, 50 ] }\n\
      ], [\n\
         \"h6\",\n\
         false,\n\
         { \"AU32\" : [ 72, 72 ] }\n\
      ]\n\
   ],\n\
   \"first\" : 583\n\
  }" in
  test_string_conv export_msg_ppp_json str

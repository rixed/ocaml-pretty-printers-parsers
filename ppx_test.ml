(* Test OCaml serialization *)

module A = struct
  type t1 = int [@@ppp PPP_OCaml]  (* <- build t1_ppp for int *)
end

module B = struct
  type t2 = int * string [@@ppp PPP_OCaml]  (* <- build t2_ppp for pair int string *)

  type t3 = { foo : int [@ppp_default 17] ;
              bar : bool ;
              recursive : t3 list [@ppp_ignore []] } [@@ppp PPP_OCaml]

  type t4 = Foo of int | Bar of string * int * bool | Baz [@@ppp PPP_OCaml]

end

type t5 = B.t4 option [@@ppp PPP_OCaml]

type t6 = PasGlop | Glop of { a:A.t1; b:t5 } [@@ppp PPP_OCaml]

type t7 = Zap of { a: t6 option } [@@ppp PPP_OCaml]

type t8 = int * string * bool * float [@@ppp PPP_OCaml]

open Stdint
type t9 = { u40 : uint40 ; i48 : int48 ; u56 : uint56 } [@@ppp PPP_OCaml]

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

type arrt = AU64 of uint64 array | AU32 of uint32 array | AFloat of float array [@@ppp PPP_JSON]
type export_msg = { first : int ; columns : (string * bool * arrt) list } [@@ppp PPP_JSON]

let () =
  let mouline ppp str =
    match PPP.of_string ppp str 0 with
    | Some (x, _) -> PPP.to_string ppp x
    | None -> "Parse error" in
  let test_string_conv ppp s =
    Printf.printf "%s: %s\n" ppp.PPP.descr (mouline ppp s) in
  test_string_conv t6_ppp "Glop { a = 42 ; b = None }" ;
  test_string_conv t6_ppp "Glop { a = 42 ; b = Some (Bar (\"bla\", 4, false)) }" ;
  test_string_conv t6_ppp "Glop{a =42; b= Some (Bar(\"bla\"  , 4, true)) }" ;
  test_string_conv t6_ppp "Glop{a=42;b=Some(Bar(\"bla\",4, true))}" ;
  test_string_conv t6_ppp "Glop { a = 42 ; b = Some (Foo 42) }" ;
  test_string_conv t6_ppp "Glop { a = 42 ; b = Some (Foo (42)) }" ;
  test_string_conv t6_ppp "Glop { a = -2 ; b = Some Baz }" ;
  test_string_conv t6_ppp "Glop { a = 0 ; b = Some(Baz) }" ;
  test_string_conv t7_ppp "Zap { a = Some PasGlop }" ;
  test_string_conv t7_ppp "Zap{ a= Some PasGlop}" ;
  test_string_conv t7_ppp "Zap{a=Some(PasGlop)}" ;
  test_string_conv t7_ppp "Zap{a=None}" ;
  test_string_conv B.t3_ppp "{ foo=42; bar=false }" ;
  test_string_conv B.t3_ppp "{ foo=42; bar =true; }" ;
  test_string_conv B.t3_ppp "{ bar =true; }" ;
  test_string_conv t8_ppp "(1, \"2\", true, 4.)" ;
  test_string_conv t9_ppp "{u40 = 42190; u56=429000 ; i48 = -42 }" ;
  test_string_conv j1_ppp "{\"field1\": 42, \"field2\": \"bla\", \"field4\": 10}" ;
  test_string_conv j1_ppp "{\"field1\": 42, \"field2\": \"bla\", \"field3\": \"z\", \"field4\": null}" ;
  test_string_conv j1_ppp "{\"field1\": 42, \"field2\": \"bla\", \"field4\": 1, \"field5\": true}" ;
  test_string_conv j2_ppp "{\"NoArg\":null}" ;
  test_string_conv j2_ppp "{\"Arg\":42}" ;
  test_string_conv j4_ppp "{ \"baz\": { \"J31\": null } }" ;
  test_string_conv j5_ppp "[42,12]" ;
  test_string_conv j5_ppp "[42, 12]" ;
  test_string_conv j5_ppp "[42 ,12]" ;
  test_string_conv j5_ppp "[ 42, 12 ] " ;
  test_string_conv j5_ppp " [42 , 12]" ;
  test_string_conv j5_ppp "  [ 42 , 12   ] " ;
  test_string_conv arrt_ppp "{ \"AFloat\" : [ 26.3129910322, 93.4604360475 ] }" ;
  test_string_conv arrt_ppp "{ \"AU64\" : [ 1493409971653419, 1493409400273526 ] }" ;
  test_string_conv export_msg_ppp "{\"columns\":[[\"h1\",false,{\"AU64\":[1]}]],\"first\":1}" ;
  test_string_conv export_msg_ppp "{\"first\":1,\"columns\":[[\"h1\",false,{\"AU64\":[1]}]]}" ;
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
  test_string_conv export_msg_ppp str

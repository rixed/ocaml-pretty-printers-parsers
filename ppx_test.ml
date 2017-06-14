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

(* Now some JSON types *)

type j1 = { field1 : int [@ppp_default 15];
            field2 : string ;
            field3 : char [@ppp_default 'x'] ;
            field4 : int option ;
            field5 : bool option [@ppp_default Some false] } [@@ppp PPP_JSON]

type j2 = NoArg | Arg of int [@@ppp PPP_JSON]

let () =
  let mouline ppp str =
    match PPP.of_string ppp str 0 with
    | Some (x, _) -> PPP.to_string ppp x
    | None -> "Parse error" in
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = None }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Bar (\"bla\", 4, false)) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop{a =42; b= Some (Bar(\"bla\"  , 4, true)) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop{a=42;b=Some(Bar(\"bla\",4, true))}") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Foo 42) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Foo (42)) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = -2 ; b = Some Baz }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 0 ; b = Some(Baz) }") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap { a = Some PasGlop }") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap{ a= Some PasGlop}") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap{a=Some(PasGlop)}") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap{a=None}") ;
  Printf.printf "<->%s\n" (mouline B.t3_ppp "{ foo=42; bar=false }") ;
  Printf.printf "<->%s\n" (mouline B.t3_ppp "{ foo=42; bar =true; }") ;
  Printf.printf "<->%s\n" (mouline B.t3_ppp "{ bar =true; }") ;
  Printf.printf "<->%s\n" (mouline t8_ppp "(1, \"2\", true, 4.)") ;
  Printf.printf "<->%s\n" (mouline j1_ppp "{\"field1\": 42, \"field2\": \"bla\", \"field4\": 10}") ;
  Printf.printf "<->%s\n" (mouline j1_ppp "{\"field1\": 42, \"field2\": \"bla\", \"field3\": \"z\", \"field4\": null}") ;
  Printf.printf "<->%s\n" (mouline j1_ppp "{\"field1\": 42, \"field2\": \"bla\", \"field4\": 1, \"field5\": true}") ;
  Printf.printf "<->%s\n" (mouline j2_ppp "{\"NoArg\":null}") ;
  Printf.printf "<->%s\n" (mouline j2_ppp "{\"Arg\":42}")

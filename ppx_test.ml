module A = struct
  type t1 = int [@@ppp PPP_block.OCaml]  (* <- build t1_ppp for int *)
end

module B = struct
  type t2 = int * string [@@ppp PPP_block.OCaml]  (* <- build t2_ppp for pair int string *)

  type t3 = { foo : int ; bar : bool ; recursive : t3 list [@ppp_ignore []] } [@@ppp PPP_block.OCaml]

  type t4 = Foo of int | Bar of string * int * bool | Baz [@@ppp PPP_block.OCaml]

end

type t5 = B.t4 option [@@ppp PPP_block.OCaml]

type t6 = PasGlop | Glop of { a:A.t1; b:t5 } [@@ppp PPP_block.OCaml]

type t7 = Zap of { a: t6 } [@@ppp PPP_block.OCaml]

type t8 = int * string * bool * float [@@ppp PPP_block.OCaml]

let () =
  let mouline ppp str =
    match PPP_block.P.of_string ppp str 0 with
    | Some (x, _) -> PPP_block.P.to_string ppp x
    | None -> "Parse error" in
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = None }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Bar (\"bla\", 4, false)) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop{a =42; b= Some (Bar(\"bla\"  , 4, true)) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop{a=42;b=Some(Bar(\"bla\",4, true))}") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Foo 42) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Foo (42)) }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = -2 ; b = Some Baz }") ;
  Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 0 ; b = Some(Baz) }") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap { a = PasGlop }") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap{ a= PasGlop}") ;
  Printf.printf "<->%s\n" (mouline t7_ppp "Zap{a=PasGlop}") ;
  Printf.printf "<->%s\n" (mouline B.t3_ppp "{ foo=42; bar=false }") ;
  Printf.printf "<->%s\n" (mouline B.t3_ppp "{ foo=42; bar =true; }") ;
  Printf.printf "<->%s\n" (mouline t8_ppp "(1, \"2\", true, 4.)")

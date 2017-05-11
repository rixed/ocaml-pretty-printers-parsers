module A = struct
  type t1 = int [@@ppp PPP_OCaml]  (* <- build t1_ppp for int *)
end

module B = struct
  type t2 = int * string [@@ppp PPP_OCaml]  (* <- build t2_ppp for pair int string *)

  type t3 = { foo : int ; bar : bool } [@@ppp PPP_OCaml]

  type t4 = Foo of int | Bar of (string * int) | Baz [@@ppp PPP_OCaml]
end

type t5 = B.t4 option [@@ppp PPP_OCaml]

type t6 = PasGlop | Glop of { a:A.t1; b:t5 } [@@ppp PPP_OCaml]

type t7 = Zap of { a: t6 } [@@ppp PPP_OCaml]

let () =
	let mouline ppp str =
    match PPP.of_string ppp str 0 with
    | Some (x, _) -> PPP.to_string ppp x
    | None -> "Parse error" in
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = None }") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Bar (\"bla\", 4)) }") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop{a =42; b= Some (Bar(\"bla\"  , 4)) }") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop{a=42;b=Some(Bar(\"bla\",4))}") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Foo 42) }") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 42 ; b = Some (Foo (42)) }") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = -2 ; b = Some Baz }") ;
	Printf.printf "<->%s\n" (mouline t6_ppp "Glop { a = 0 ; b = Some(Baz) }") ;
	Printf.printf "<->%s\n" (mouline t7_ppp "Zap { a = PasGlop }") ;
	Printf.printf "<->%s\n" (mouline t7_ppp "Zap{ a= PasGlop}") ;
	Printf.printf "<->%s\n" (mouline t7_ppp "Zap{a=PasGlop}")

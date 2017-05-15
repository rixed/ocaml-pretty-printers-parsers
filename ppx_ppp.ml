(* Abandon all hope ye who enter here *)
open Parsetree
open Ast_helper

let exp_todo n =
  Exp.constant (Const.string ("TODO:"^n))

let extract_ident_attribute n attrs =
  let rec loop prev = function
    | [] -> None
    | ({ Asttypes.txt ; _ }, (* Match identifier (to get module name for @@ppp *)
       PStr [
         { pstr_desc =
             Pstr_eval ({
               pexp_desc =
                 Pexp_construct (
                   { Asttypes.txt = Longident.Lident ident; _ }, _) ;
               _ }, _) ;
           _ }
       ] )::rest when txt = n ->
      Some (ident, List.rev_append prev rest)
    | x::rest ->
      loop (x :: prev) rest in
  loop [] attrs

let extract_expr_attribute n attrs =
  let rec loop prev = function
    | [] -> None
    | ({ Asttypes.txt ; _ }, (* Match expressions (to get default values *)
       PStr [
         { pstr_desc = Pstr_eval (exp, _) ; _ }
       ] )::rest when txt = n ->
      Some (exp, List.rev_append prev rest)
    | x::rest ->
      loop (x :: prev) rest in
  loop [] attrs

let loc_of x =
  Asttypes.{ txt = x ; loc = !default_loc }

let ident_of_name name =
  loc_of (Longident.Lident name)

(* returns an attribute *)
let disable_warnings ns =
  let v = List.fold_left (fun s n -> s^"-"^ string_of_int n) "" ns in
  loc_of "ocaml.warning",
  PStr [ (
    Str.eval (Exp.constant (Pconst_string (v, None)))) ]

let exp_of_constr name opt =
  Exp.construct (ident_of_name name) opt

let exp_of_bool = function
  | true -> exp_of_constr "true" None
  | false -> exp_of_constr "false" None

let exp_of_unit = exp_of_constr "()" None

let exp_of_ppp_exception name =
  Exp.construct (loc_of Longident.(Ldot (Lident "PPP", name))) None

let exp_of_name x =
  Exp.ident (ident_of_name x)

let list_init n f =
  let rec loop prev i =
    if i >= n then List.rev prev else
    loop (f i :: prev) (i + 1)
  in
  loop [] 0

let list_reduce f lst =
  let rec loop prev = function
    | [] -> prev
    | x::rest ->
      loop (f prev x) rest
  in
  match lst with
    | [] -> invalid_arg "list_reduce"
    | [x] -> x
    | x::rest -> loop x rest

let list_of_n_names n x =
  assert (n > 0) ;
  list_init n (fun i ->
    exp_of_name (x ^ string_of_int i))

let exp_of_n_names n x =
  assert (n > 0) ;
  if n = 1 then exp_of_name x else
  Exp.tuple (list_of_n_names n x)

let field_exp_of_name rec_name field_name =
  Exp.field (exp_of_name rec_name) (ident_of_name field_name)

let pattern_of_var x =
  Pat.var (loc_of x)

let pattern_of_n_vars n x =
  assert (n > 0) ;
  if n = 1 then pattern_of_var x else
  Pat.tuple (list_init n (fun i ->
    pattern_of_var (x ^ string_of_int i)))

let pattern_of_constr name opt =
  Pat.construct (ident_of_name name) opt

let pattern_catch_all exp =
  { pc_lhs = Pat.any () ;
    pc_guard = None ;
    pc_rhs = exp }

let value_binding_of_expr name expr =
  Vb.mk ~attrs:[disable_warnings [8; 27]] (pattern_of_var name) expr

let name_of_ppp n = n ^"_ppp"

let ppp_name_of_name = function
  | ("bool" | "int" | "int32" | "int64" | "float" |
     "string" | "unit" | "none" | "list" | "array" |
     "option" | "uint32" | "uint64") as x -> x
  | x -> name_of_ppp x

let ppp_ident_of_ident = function
  | Longident.Lident str -> Longident.Lident (ppp_name_of_name str)
  | Longident.Ldot (pref, str) -> Longident.Ldot (pref, ppp_name_of_name str)
  | _ -> failwith "Lapply?"

let ppp_exp_of_ident ident =
  ppp_ident_of_ident ident |> loc_of |> Exp.ident

let apply_expr f_expr param_exprs =
  let params = List.map (fun exp -> Asttypes.Nolabel, exp) param_exprs in
  Exp.apply f_expr params

let apply f_name param_exprs =
  let f_expr = exp_of_name f_name in
  apply_expr f_expr param_exprs

let apply1 f_name p =
  apply f_name [ p ]

let apply2 f_name p1 p2 =
  apply f_name [ p1 ; p2 ]

let rec ppp_exp_of_tuple core_types =
  let rec add_type prev = function
    | [] -> (* close the tuple *)
      apply "+-" [
        prev ;
        apply "cst" [ exp_of_name "tuple_close" ]
      ]
    | core_type::rest ->
      let ppp = ppp_exp_of_core_type core_type in
      let item =
        apply "-+" [
          apply "cst" [ exp_of_name "tuple_sep" ] ;
          ppp ] in
      add_type
        (apply "++" [ prev ; item ])
        rest in
  match core_types with
  | [core_type] ->
    ppp_exp_of_core_type core_type
  | core_type::rest ->
    let ppp = ppp_exp_of_core_type core_type in
    let tree =
      apply "-+" [
        apply "cst" [ exp_of_name "tuple_open" ] ;
        add_type ppp rest ] in
    let nb_types = List.length core_types in
    assert (nb_types >= 2) ;
    if nb_types = 2 then tree else
    apply2 ">>:" tree (
      (* We need to flatten that tree into a tuple *)
      let tuple_pat = pattern_of_n_vars nb_types "x"
      and tuple_exp = exp_of_n_names nb_types "x"
      and tree_pat = leftist_tree_all_pattern ~options:false nb_types
      and tree_exp = leftist_tree_all_expr ~options:false (list_of_n_names nb_types "x")
      in
      Exp.tuple [
        Exp.fun_ Asttypes.Nolabel None tuple_pat tree_exp ;
        Exp.fun_ Asttypes.Nolabel None tree_pat tuple_exp ]
    )
  | [] ->
    exp_of_name "none"

and ppp_exp_of_core_type core_type =
  match core_type.ptyp_desc with
  (* Tuples *)
  | Ptyp_tuple core_types when core_types <> [] ->
    ppp_exp_of_tuple core_types
  (* Constructors *)
  | Ptyp_constr ({ Asttypes.txt = lident_base_type ; _ }, []) ->
    ppp_exp_of_ident lident_base_type
  | Ptyp_constr ({ Asttypes.txt = lident_base_type ; _ }, params) ->
    assert (params <> []) ;
    (* for instance we have: (int, string) Hashtbl.t. We want : PPP.(hashtbl int string) *)
    apply_expr (ppp_exp_of_ident lident_base_type) (List.map (fun param ->
        ppp_exp_of_core_type param) params)
  (* Aliases *)
  | Ptyp_alias (core_type, _name) ->
    ppp_exp_of_core_type core_type
  (* Polymorphic variants *)
  | Ptyp_variant (fields, _closed_flag, labels_opt) ->
    exp_todo "Polymorphic variants"
  | _ ->
    exp_todo "Some obscure core_type"

(* returns a pattern with Some variable for each spot: *)
and leftist_tree_all_pattern ~options nb_vars =
  let some_of p =
    if options then pattern_of_constr "Some" (Some p)
    else p in
  let some_of_var i =
    some_of (pattern_of_var ("x"^ string_of_int i)) in
  let rec loop first prev i =
    if i >= nb_vars then prev else (
      let prev = Pat.tuple [
        if first then prev else some_of prev ;
        some_of_var i ] in
      loop false prev (i+1)
    )
  in
  assert (nb_vars > 1) ;
  let prev = some_of_var 0 in
  loop true prev 1

(* Returns an leftist option tree expression made of record x labels *)
and leftist_tree_all_expr ~options exps =
  let some_of exp =
    if options then exp_of_constr "Some" (Some exp)
    else exp in
  let rec loop first prev = function
    | [] -> prev
    | x::rest ->
      let prev = Exp.tuple [
        if first then prev else some_of prev ;
        some_of x ] in
      loop false prev rest
  in
  assert (exps <> []) ;
  let prev = some_of (List.hd exps) in
  loop true prev (List.tl exps)

let field_exp_of_label_decl label_decl =
  apply2 "field"
    (Exp.constant (
      Const.string
        label_decl.pld_name.Asttypes.txt))
    (ppp_exp_of_core_type label_decl.pld_type)

let exp_of_label_decls ?constr_name label_decls =
  (* Some labels may be ignored: *)
  let ignored_labels, not_ignored_labels =
    List.fold_left (fun (ign, not_ign) label_decl ->
        match extract_expr_attribute "ppp_ignore" label_decl.pld_attributes with
        | None -> ign, (label_decl :: not_ign)
        | Some attr -> ((label_decl, attr) :: ign), not_ign
      ) ([], []) label_decls in
  (* For sanity: *)
  let ignored_labels = List.rev ignored_labels
  and not_ignored_labels = List.rev not_ignored_labels in
  let default_of_field (label_decl, (v, _)) =
    ident_of_name label_decl.pld_name.Asttypes.txt,
    v in
  apply2 ">>:" (
    apply1 "record" (
      (* For each field, emit a field expression *)
      List.map field_exp_of_label_decl not_ignored_labels |>
      (* Connect all the variants with ||| operator *)
      list_reduce (apply2 "<->"))
  ) (
    (* When the record is part of a constructor we cannot pattern-match the record fields
     * as usual and have to include that constructor with the pattern ; effectively we do
     * no pattern-match a record (which is not defined) but a constructor. *)
    let maybe_constr_pattern pat =
      match constr_name with
      | None -> pat
      | Some cn -> pattern_of_constr cn (Some pat)
    and maybe_constr_record expr =
      match constr_name with
      | None -> expr
      | Some cn -> exp_of_constr cn (Some expr) in
    let nb_labels = List.length not_ignored_labels in
    if nb_labels = 1 then (
      (* Special case: no need to go through a tuple of options, all we need
         is to get rid of the label or add it. *)
      let label_decl = List.hd not_ignored_labels in
      let label_name = label_decl.pld_name.Asttypes.txt in
      Exp.tuple [
        Exp.function_ [
          {
            pc_lhs = maybe_constr_pattern (pattern_of_var "x") ;
            pc_guard = None ;
            pc_rhs = field_exp_of_name "x" label_name
          }] ;
        Exp.function_ [
          {
            pc_lhs = pattern_of_var "x" ;
            pc_guard = None ;
            pc_rhs = maybe_constr_record (Exp.record (
              (ident_of_name label_name, exp_of_name "x") ::
              List.map default_of_field ignored_labels
            ) None (* ? *))
          }]]
    ) else (
      assert (nb_labels >= 2) ;
      Exp.tuple [
        Exp.function_ [
          {
            pc_lhs = maybe_constr_pattern (pattern_of_var "x") ;
            pc_guard = None ;
            pc_rhs =
              leftist_tree_all_expr ~options:true
                (List.map (fun label_decl ->
                  field_exp_of_name "x" label_decl.pld_name.Asttypes.txt)
                  not_ignored_labels)
          }] ;
        (* No support for default values (yet!) *)
        Exp.function_ ([
          {
            pc_lhs = leftist_tree_all_pattern ~options:true nb_labels ;
            pc_guard = None ;
            pc_rhs = maybe_constr_record (Exp.record (
                  List.mapi (fun i label_decl ->
                      ident_of_name label_decl.pld_name.Asttypes.txt,
                      exp_of_name ("x"^ string_of_int i)
                    ) not_ignored_labels @
                  List.map default_of_field ignored_labels
                ) None)
          }] @ [
            pattern_catch_all
              (apply1 "raise" (exp_of_ppp_exception "MissingRequiredField"))
          ]) ]))

let variant_exp_of_constructor_decl constructor_decl =
  apply2 "variant"
    (Exp.constant (
      Const.string
        constructor_decl.pcd_name.Asttypes.txt))
    (match constructor_decl.pcd_res with
    | Some core_type -> (* GATD? *) exp_todo "GATD?"
    | None -> (* then a constructed type *)
      (match constructor_decl.pcd_args with
      | Pcstr_tuple lst -> ppp_exp_of_tuple lst
      | Pcstr_record lst ->
        let constr_name = constructor_decl.pcd_name.Asttypes.txt in
        exp_of_label_decls ~constr_name lst))

(* Receive [ Some "a"; Some "b"; None ] and returns the pattern for
 * Some (Some a, Some b), _ *)
let leftist_option_tree_mono_pattern variable_names =
  let some_of p = pattern_of_constr "Some" (Some p) in
  let some_of_var = function
    | None -> Pat.any (), true
    | Some (nb_args, x) ->
      some_of (if nb_args > 0 then pattern_of_n_vars nb_args x else Pat.any ()),
      false in
  let rec loop first prev prev_is_any = function
    | [] -> prev
    | x::rest -> (* Some rest, Some x unless that's our first pair: rest, Some x *)
      let new_, new_is_any = some_of_var x in
      let prev, prev_is_any =
        if new_is_any && prev_is_any then prev, true else
          Pat.tuple [
            if prev_is_any || first then prev else some_of prev ;
            new_ ], false in
      loop false prev prev_is_any rest
  in
  assert (variable_names <> []) ;
  let prev, prev_is_any = some_of_var (List.hd variable_names) in
  loop true prev prev_is_any (List.tl variable_names)

(* Return the expression for the leftist tree with all None but the ith value *)
let leftist_option_tree_mono_expr nb_args var_with_value value =
  let some_of x = exp_of_constr "Some" (Some x)
  and none () = exp_of_constr "None" None in
  let some_or_none i =
    if i = var_with_value then
      some_of value, false
    else none (), true in
  let rec loop i prev prev_is_none =
    if i >= nb_args then prev
    else (
      let new_, new_is_none = some_or_none i in
      (* Some prev, new  unless prev was None in which case: None, new
       * or prev was not a pair (if i=1) in which case: prev, new *)
      let prev, prev_is_none =
        if new_is_none && prev_is_none then prev, true else
        Exp.tuple [
          if prev_is_none || i = 1 then prev else some_of prev ;
          new_ ], false in
      loop (i+1) prev prev_is_none
    ) in
  assert (nb_args > 0) ;
  let new_, new_is_none =
    some_or_none 0 in
  loop 1 new_ new_is_none

let construct_nb_args constructor_decl =
  match constructor_decl.pcd_args with
  | Pcstr_tuple lst -> List.length lst
  | Pcstr_record lst -> List.length lst

let exp_of_constructor_arguments constructor_decls =
  let construct_has_record = function
    | { pcd_args = Pcstr_record _ ; _ } -> true
    | _ -> false in
  apply2 ">>:" (
    apply1 "union" (
      (* For each constructor, emit a variant expression *)
      List.map variant_exp_of_constructor_decl constructor_decls |>
      (* Connect all the variants with ||| operator *)
      list_reduce (apply2 "|||"))
  ) (
    let nb_consts = List.length constructor_decls in
    if nb_consts = 1 then (
      (* Special case: no need to go through a tuple of options, all we need
         is to get rid of the constructor or add it. *)
      let constructor_decl = List.hd constructor_decls in
      let constr_name = constructor_decl.pcd_name.Asttypes.txt in
      let nb_args = if construct_has_record constructor_decl then 1
                    else construct_nb_args constructor_decl in
      let has_args = nb_args > 0 in
      Exp.tuple [
        Exp.function_ [
          {
            pc_lhs =
							(if construct_has_record constructor_decl then
                pattern_of_var "x" else
              Pat.construct (ident_of_name constr_name)
                (if has_args then Some (pattern_of_n_vars nb_args "x") else None)) ;
            pc_guard = None ;
            pc_rhs = if has_args then exp_of_n_names nb_args "x" else failwith "not implemented"
          }] ;
        Exp.function_ [
          {
            pc_lhs = if has_args then pattern_of_n_vars nb_args "x" else failwith "not implemented" ;
            pc_guard = None ;
            pc_rhs =
							if construct_has_record constructor_decl then
                exp_of_name "x" else
              exp_of_constr constr_name
                (if has_args then Some (exp_of_n_names nb_args "x") else None)
          }]]
    ) else (
      assert (nb_consts >= 2) ;
      (* Convert from/to the pair of pairs of options *)
      (* We need:
           function cstrs x -> left-ist tree of pairs of optional values
                                with the nth set to Some x
         and:
           function left-ist tree -> cstr x *)
      (* With the additional difficulty that constructor with a record attached
         have to be passed with their record. *)
      Exp.tuple [
        Exp.function_ (List.mapi (fun i constructor_decl ->
          let constr_name = constructor_decl.pcd_name.Asttypes.txt in
          let nb_args = if construct_has_record constructor_decl then 1
                        else construct_nb_args constructor_decl in
          {
            pc_lhs =
              pattern_of_constr constr_name
                (if nb_args > 0 then
                  Some (pattern_of_n_vars nb_args "x") else None) ;
            pc_guard = None ;
            pc_rhs =
              let my_value =
                (* TODO: could we not do this systematically? *)
                if construct_has_record constructor_decl then
                  exp_of_constr constr_name (Some (exp_of_name "x"))
                else if nb_args > 0 then
                  exp_of_n_names nb_args "x"
                else exp_of_unit in
              leftist_option_tree_mono_expr nb_consts i my_value
          }) constructor_decls) ;
        Exp.function_ ((List.mapi (fun i constructor_decl ->
          let constr_name = constructor_decl.pcd_name.Asttypes.txt in
          let nb_args = if construct_has_record constructor_decl then 1
                        else construct_nb_args constructor_decl in
          let patterned_vars = List.mapi (fun j constructor_decl ->
              if j = i then Some (nb_args, "x") else None
            ) constructor_decls in
          {
            pc_lhs = leftist_option_tree_mono_pattern patterned_vars ;
            pc_guard = None ;
            pc_rhs =
              if construct_has_record constructor_decl then
                exp_of_name "x"
              else exp_of_constr constr_name
                (if nb_args > 0 then
                   Some (exp_of_n_names nb_args "x") else None)
          }) constructor_decls)) ]))

let ppp_of_type_declaration tdec =
  match extract_ident_attribute "ppp" tdec.ptype_attributes with
  | None -> (* don't care *)
    tdec, None
  | Some (var_module, attrs) ->
    let replacement = { tdec with ptype_attributes = attrs } in
    let impl_mod = ident_of_name var_module in
    replacement, (match tdec.ptype_manifest with
    | None ->
      (match tdec.ptype_kind with
      | Ptype_variant constructor_decls ->
        Some (
          value_binding_of_expr
            (name_of_ppp tdec.ptype_name.Asttypes.txt)
            (Exp.open_ Asttypes.Override impl_mod
              (exp_of_constructor_arguments constructor_decls)))
      | Ptype_record label_decls ->
        Some (
          value_binding_of_expr
            (name_of_ppp tdec.ptype_name.Asttypes.txt)
            (Exp.open_ Asttypes.Override impl_mod
              (exp_of_label_decls label_decls)))
      | _ -> (* Nope *)
        None)
    | Some core_type ->
      Some (
        value_binding_of_expr
          (name_of_ppp tdec.ptype_name.Asttypes.txt)
          (Exp.open_ Asttypes.Override impl_mod
            (ppp_exp_of_core_type core_type))))

let map_structure mapper str =
  let strs = Ast_mapper.default_mapper.Ast_mapper.structure mapper str in
	(* Try to emit the new ppp definitions just after the corresponding type
     definition: *)
  List.fold_left (fun strs' str ->
      match str.pstr_desc with
      | Pstr_type (rec_flag, type_decls) ->
        let str_to_emit = ref [] in
        let new_type_decls =
          List.map (fun type_decl ->
              match ppp_of_type_declaration type_decl with
              | tdec, None -> tdec
              | tdec, Some vb ->
                let s = Str.value Asttypes.Nonrecursive [vb] in
                str_to_emit := s::!str_to_emit ;
                tdec
            ) type_decls in
        let str = Str.type_ rec_flag new_type_decls in
        List.rev_append !str_to_emit (str::strs')
      | _ ->
        str::strs'
    ) [] strs |> List.rev

let ppp_mapper _argv =
  { Ast_mapper.default_mapper with
    Ast_mapper.structure = map_structure }

let () =
  Ast_mapper.register "ppx_ppp" ppp_mapper

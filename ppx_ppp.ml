(* Abandon all hope ye who enter here *)
open Parsetree
open Ast_helper

let exp_todo n =
  Exp.constant (Const.string ("TODO:"^n))

(* Return a possibly empty list of implementation modules (longidents), a
 * flag telling if the ppp is extensible and the remaining attributes. *)
let extract_ident_attribute attrs =
  let rec loop mods extensible others = function
    | [] -> mods, extensible, List.rev others
    | { attr_name = { Asttypes.txt = "ppp" ; _ } ;
        attr_payload =
         (* Match identifier (to get module name for @@ppp *)
         PStr [
           { pstr_desc =
               Pstr_eval ({
                 pexp_desc =
                   Pexp_construct (ident, _) ;
                 _ }, _) ;
             _ } ] ;
        _ } :: rest ->
        loop (ident::mods) extensible others rest
    | { attr_name = { Asttypes.txt = "ppp_extensible" ; _ } ;
        attr_payload = PStr [] ; _ } :: rest ->
        loop mods true others rest
    | attr :: rest ->
        loop mods extensible (attr :: others) rest in
  loop [] false [] attrs

let extract_expr_attribute n attrs =
  let rec loop others = function
    | [] -> None
    | { attr_name = { Asttypes.txt ; _ } ; (* Match expressions (to get default values *)
        attr_payload = PStr [ { pstr_desc = Pstr_eval (exp, _) ; _ } ] ;
        _ } :: rest when txt = n ->
      Some (exp, List.rev_append others rest)
    | x::rest ->
      loop (x :: others) rest in
  loop [] attrs

let loc_of x =
  Asttypes.{ txt = x ; loc = !default_loc }

let ident_of_name name =
  loc_of (Longident.Lident name)

(* returns an attribute *)
let disable_warnings ns =
  let v = List.fold_left (fun s n -> s^"-"^ string_of_int n) "" ns in
  { attr_name = loc_of "ocaml.warning" ;
    attr_payload =
      PStr [ (Str.eval (Exp.constant (Pconst_string (v, None)))) ] ;
    attr_loc = Location.none }

let exp_of_constr name opt =
  Exp.construct (ident_of_name name) opt

let exp_of_bool = function
  | true -> exp_of_constr "true" None
  | false -> exp_of_constr "false" None

let exp_of_unit = exp_of_constr "()" None

let exp_of_ppp_exception name value =
  Exp.construct (loc_of Longident.(Ldot (Lident "PPP", name))) value

let exp_of_name x =
  Exp.ident (ident_of_name x)

let exp_of_impl_name impl_mod x =
  Exp.ident (loc_of Longident.(Ldot (impl_mod.Asttypes.txt, x)))

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

let pattern_none = pattern_of_constr "None" None
let pattern_unit = pattern_of_constr "()" None

let value_binding_of_expr name expr =
  (* eta-expanse the applied [expr] *)
  let fun_expr = Exp.fun_ Asttypes.Nolabel None pattern_unit (Exp.apply expr [ Asttypes.Nolabel, exp_of_unit ]) in
  Vb.mk ~attrs:[disable_warnings [11; 12; 27; 39]] (pattern_of_var name) fun_expr

let identifier_of_mod modident =
  Longident.flatten modident.Asttypes.txt |>
  String.concat "_" |>
  String.lowercase_ascii

let name_of_ppp impl_mod n =
  n ^"_"^ identifier_of_mod impl_mod

let ppp_name_of_name impl_mod = function
  | ("bool" | "char" | "int" | "float" | "string" | "unit" |
     "int8" | "int16" | "int24" | "int32" | "int40" |
     "int48" | "int56" | "int64" | "int128" |
     "uint8" | "uint16" | "uint24" | "uint32" | "uint40" |
     "uint48" | "uint56" | "uint64" | "uint128" |
     "none" | "list" | "array" | "option" | "ref") as x -> true, x
  | x -> false, name_of_ppp impl_mod x

let ppp_ident_of_ident impl_mod = function
  | Longident.Lident str ->
      let reserved, ppp_name = ppp_name_of_name impl_mod str in
      if reserved then
        Longident.Ldot (impl_mod.Asttypes.txt, ppp_name)
      else
        Longident.Lident ppp_name
  | Longident.Ldot (Longident.Lident "Hashtbl", "t") ->
      Longident.Ldot (impl_mod.Asttypes.txt, "hashtbl")
  | Longident.Ldot (Longident.Lident "Unix", "inet_addr") ->
      Longident.Ldot (Longident.Lident "PPP_Unix", "inet_addr")
  (* TODO: also: List.t, Array.t... *)
  | Longident.Ldot (pref, str) ->
      let reserved, ppp_name = ppp_name_of_name impl_mod str in
      if reserved then
        Printf.eprintf "Use standard type name %s with module %s\n"
          str (String.concat "." (Longident.flatten pref)) ;
      Longident.Ldot (pref, ppp_name)
  | _ -> failwith "Lapply?"

let ppp_exp_of_ident impl_mod ident =
  ppp_ident_of_ident impl_mod ident |> loc_of |> Exp.ident

let apply_expr_ f_expr params =
  Exp.apply f_expr params

let apply_expr f_expr param_exprs =
  let params = List.map (fun exp -> Asttypes.Nolabel, exp) param_exprs in
  apply_expr_ f_expr params

(* Apply the params to the impl_mod function named f_name *)
let apply impl_mod f_name param_exprs =
  let f_expr = exp_of_impl_name impl_mod f_name in
  apply_expr f_expr param_exprs

let raise_ e =
  let raise_expr = exp_of_name "raise" in
  Exp.apply raise_expr [ Asttypes.Nolabel, e ]

let apply1 impl_mod f_name p =
  apply impl_mod f_name [ p ]

let apply2 impl_mod f_name p1 p2 =
  apply impl_mod f_name [ p1 ; p2 ]

let rec ppp_exp_of_tuple impl_mod core_types =
  let rec add_type prev = function
    | [] -> (* close the tuple *)
      apply impl_mod "+-" [
        prev ;
        apply impl_mod "cst" [ exp_of_impl_name impl_mod "tuple_close" ]
      ]
    | core_type::rest ->
      let ppp = ppp_exp_of_core_type impl_mod core_type in
      let item =
        apply impl_mod "-+" [
          apply impl_mod "cst" [ exp_of_impl_name impl_mod "tuple_sep" ] ;
          ppp ] in
      add_type
        (apply impl_mod "++" [ prev ; item ])
        rest in
  match core_types with
  | [core_type] ->
    ppp_exp_of_core_type impl_mod core_type
  | core_type::rest ->
    let ppp = ppp_exp_of_core_type impl_mod core_type in
    let tree =
      apply impl_mod "-+" [
        apply impl_mod "cst" [ exp_of_impl_name impl_mod "tuple_open" ] ;
        add_type ppp rest ] in
    let nb_types = List.length core_types in
    assert (nb_types >= 2) ;
    if nb_types = 2 then tree else
    apply2 impl_mod ">>:" tree (
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
    exp_of_impl_name impl_mod "none"

and ppp_exp_of_core_type impl_mod core_type =
  match core_type.ptyp_desc with
  (* Tuples *)
  | Ptyp_tuple core_types when core_types <> [] ->
    ppp_exp_of_tuple impl_mod core_types
  (* Constructors *)
  | Ptyp_constr ({ Asttypes.txt = lident_base_type ; _ }, []) ->
    ppp_exp_of_ident impl_mod lident_base_type
  | Ptyp_constr ({ Asttypes.txt = lident_base_type ; _ }, params) ->
    assert (params <> []) ;
    (* for instance we have: (int, string) Hashtbl.t. We want : PPP.(hashtbl int string) *)
    apply_expr (ppp_exp_of_ident impl_mod lident_base_type) (List.map (fun param ->
        ppp_exp_of_core_type impl_mod param) params)
  (* Aliases *)
  | Ptyp_alias (core_type, _name) ->
    ppp_exp_of_core_type impl_mod core_type
  (* Polymorphic variants *)
  | Ptyp_variant (_fields, _closed_flag, _labels_opt) ->
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

(* [nb_vars] is the number of variables in the leftist tree, and [none_pos] the index
 * of the one missing. Notice that we also want to match the case where several might
 * be missing, although it is enough to err on the first (or any) one.
 * Also, when several fields are missing, it might happen that a whole branch of the
 * tree is missing (Some (None, None) <=> None). Failing to match that would yield
 * to elusive runtime match errors depending on what fields are missing. *)
and leftist_tree_none_at nb_vars none_pos =
  (* So each time we have "Some (None, _)" or "Some (_, None)" we want to also
   * allow for "None". This will create some redundancy therefore the warning 12
   * has to be disabled: *)
  let some_of p =
    match p.ppat_desc with
    | Ppat_tuple [ { ppat_desc = Ppat_any ; _ } ;
                   _ (* this we know can be nothing else than None *) ]
    | Ppat_tuple [ _ ;
                   { ppat_desc = Ppat_any ; _ } ] ->
        Pat.or_ (pattern_of_constr "Some" (Some p))
                pattern_none
    | _ -> pattern_of_constr "Some" (Some p) in
  let pat_of i =
    if i = none_pos then pattern_none
    else Pat.any () in
  let rec loop prev i =
    if i >= nb_vars then prev else
    let prev = Pat.tuple [
      (if i = 1 then prev
      else if i <= none_pos then Pat.any ()
      else some_of prev) ;
      pat_of i ] in
    loop prev (i + 1)
  in
  loop (pat_of 0) 1

(* Returns a leftist option tree expression made of record x labels *)
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

let string_of_exp exp =
  match exp.pexp_desc with
  | Pexp_constant (Pconst_string (s, _)) -> s
  | _ ->
    Printf.eprintf "Invalid ppp_rename: must provide a string constant.\n%!" ;
    "INVALID_PPP_RENAME"

let assert_not_any =
  { pc_lhs = Pat.any () ;
    pc_guard = None ;
    pc_rhs = Exp.assert_ (exp_of_bool false) }

let exp_of_label_decls ?constr_name ~extensible impl_mod label_decls =
  (* Some labels may be ignored: *)
  let ignored_labels, not_ignored_labels =
    List.fold_left (fun (ign, not_ign) label_decl ->
        match extract_expr_attribute "ppp_ignore" label_decl.pld_attributes with
        | None ->
          let default = extract_expr_attribute "ppp_default" label_decl.pld_attributes
          and rename =
            match extract_expr_attribute "ppp_rename" label_decl.pld_attributes with
            | Some (rename_exp, _) -> string_of_exp rename_exp
            | None -> label_decl.pld_name.Asttypes.txt in
          (* TODO: each time the record field is an option we should have an implicit default none *)
          ign, ((label_decl, default, rename) :: not_ign)
        | Some attr -> ((label_decl, attr) :: ign), not_ign
      ) ([], []) label_decls in
  (* For sanity: *)
  let ignored_labels = List.rev ignored_labels
  and not_ignored_labels = List.rev not_ignored_labels in
  let default_of_field (label_decl, (v, _)) =
    ident_of_name label_decl.pld_name.Asttypes.txt,
    v in
  let field_exp_of_label_decl = function
    | label_decl, None, label_name ->
      apply2 impl_mod "field"
        (Exp.constant (Const.string label_name))
        (ppp_exp_of_core_type impl_mod label_decl.pld_type)
    | label_decl, Some (v, _), label_name ->
      let params = [
        Asttypes.Optional "default", exp_of_constr "Some" (Some v) ;
        Asttypes.Nolabel, Exp.constant (Const.string label_name) ;
        Asttypes.Nolabel, ppp_exp_of_core_type impl_mod label_decl.pld_type ] in
      apply_expr_ (exp_of_impl_name impl_mod "field") params in
  apply2 impl_mod ">>:" (
    let fields =
      (* For each field, emit a field expression *)
      List.map field_exp_of_label_decl not_ignored_labels |>
      (* Connect all the variants with ||| operator *)
      list_reduce (apply2 impl_mod "<->") in
    apply_expr_ (exp_of_impl_name impl_mod "record")
              [ Asttypes.Optional "extensible",
                  if extensible then
                    exp_of_constr "Some" (Some (exp_of_bool true))
                  else
                    exp_of_constr "None" None ;
                Asttypes.Nolabel, fields ]
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
      | Some cn -> exp_of_constr cn (Some expr)
    (* If this is indeed the record attached to a constructor, it is safe to
     * assume that there are other possible constructors (warning 11 is
     * disabled in case there are only one). Those should in theory never be
     * presented to this match, but to avoid a warning (and make errors easier
     * to diagnose) crash when this is the case: *)
    and maybe_ignore_other_constr =
      match constr_name with
      | None -> []
      | Some _ -> [ assert_not_any ]
    in
    let nb_labels = List.length not_ignored_labels in
    if nb_labels = 1 then (
      (* Special case: no need to go through a tuple of options, all we need
         is to get rid of the label or add it. *)
      let label_decl, _, _ = List.hd not_ignored_labels in
      let label_name = label_decl.pld_name.Asttypes.txt in
      Exp.tuple [
        Exp.function_ ([
          {
            pc_lhs = maybe_constr_pattern (pattern_of_var "x") ;
            pc_guard = None ;
            pc_rhs = field_exp_of_name "x" label_name
          }] @ maybe_ignore_other_constr);
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
        Exp.function_ ([
          {
            pc_lhs = maybe_constr_pattern (pattern_of_var "x") ;
            pc_guard = None ;
            pc_rhs =
              leftist_tree_all_expr ~options:true
                (List.map (fun (label_decl, _, _) ->
                  field_exp_of_name "x" label_decl.pld_name.Asttypes.txt)
                  not_ignored_labels)
          }] @ maybe_ignore_other_constr) ;
        Exp.function_ ([
          (* First pattern match the case where we have all the fields (because
           * they were present explicitly or had a default value: *)
          {
            pc_lhs = leftist_tree_all_pattern ~options:true nb_labels ;
            pc_guard = None ;
            pc_rhs = maybe_constr_record (Exp.record (
                  List.mapi (fun i (label_decl, _, _) ->
                      ident_of_name label_decl.pld_name.Asttypes.txt,
                      exp_of_name ("x"^ string_of_int i)
                    ) not_ignored_labels @
                  List.map default_of_field ignored_labels
                ) None)
          }] @ (
            (* Then all the patterns with missing required fields.
             * We could have the name of the first missing field if we
             * matched against 'None, _', then 'Some _, None, _', etc *)
            List.mapi (fun i (_label_decl, _, rename) ->
              {
                pc_lhs = leftist_tree_none_at nb_labels i ;
                pc_guard = None ;
                pc_rhs = raise_ (exp_of_ppp_exception "MissingRequiredField" (Some (Exp.constant (Const.string rename))))
              }
            ) not_ignored_labels
          )) ]))

let variant_exp_of_constructor_decl impl_mod constructor_decl =
  apply2 impl_mod "variant"
    (Exp.constant (
      Const.string
        constructor_decl.pcd_name.Asttypes.txt))
    (match constructor_decl.pcd_res with
    | Some _core_type -> (* GATD? *) exp_todo "GATD?"
    | None -> (* then a constructed type *)
      (match constructor_decl.pcd_args with
      | Pcstr_tuple lst -> ppp_exp_of_tuple impl_mod lst
      | Pcstr_record lst ->
        let constr_name = constructor_decl.pcd_name.Asttypes.txt in
        exp_of_label_decls ~extensible:false ~constr_name impl_mod lst))

(* Receive [ Some "a"; Some "b"; None ] and returns the pattern for
 * Some (Some a, Some b), None *)
let leftist_option_tree_mono_pattern variable_names =
  let some_of p = pattern_of_constr "Some" (Some p) in
  let some_of_var = function
    | None -> pattern_none, true
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

let exp_of_constructor_arguments impl_mod constructor_decls =
  let construct_has_record = function
    | { pcd_args = Pcstr_record _ ; _ } -> true
    | _ -> false in
  apply2 impl_mod ">>:" (
    apply1 impl_mod "union" (
      (* For each constructor, emit a variant expression *)
      List.map (variant_exp_of_constructor_decl impl_mod) constructor_decls |>
      (* Connect all the variants with ||| operator *)
      list_reduce (apply2 impl_mod "|||"))
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
        Exp.function_ (
          List.mapi (fun i constructor_decl ->
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
            }
          ) constructor_decls) ;
        Exp.function_ (
          List.mapi (fun i constructor_decl ->
            let constr_name = constructor_decl.pcd_name.Asttypes.txt in
            let nb_args = if construct_has_record constructor_decl then 1
                          else construct_nb_args constructor_decl in
            let patterned_vars = List.mapi (fun j _constructor_decl ->
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
            }
          ) constructor_decls @ [
            (* Although we cannot have another pattern here given the above
             * patterns mirror comprehensively all the structures created by
             * the parser above, let's make it crash otherwise, so that we are
             * 200% sure any pattern failure come from missing fields in records: *)
            assert_not_any ]) ]))

let ppps_of_type_declaration tdec =
  match extract_ident_attribute tdec.ptype_attributes with
  | [], false, _ -> (* don't care *)
    tdec, []
  | [], true, _ -> (* a bit weird *)
    Printf.eprintf "Used ppp_extensible but no PPP module given.\n%!" ;
    tdec, []
  | impl_mods, extensible, other_attrs ->
    let replacement = { tdec with ptype_attributes = other_attrs } in
    replacement, List.fold_left (fun vbs impl_mod ->
      match tdec.ptype_manifest with
      | None ->
        (match tdec.ptype_kind with
        | Ptype_variant constructor_decls ->
          if extensible then
            Printf.eprintf "Used ppp_extensible on a variant but only \
                            records are extensible.\n%!" ;
          value_binding_of_expr
            (name_of_ppp impl_mod tdec.ptype_name.Asttypes.txt)
            (exp_of_constructor_arguments impl_mod constructor_decls) :: vbs
        | Ptype_record label_decls ->
          value_binding_of_expr
            (name_of_ppp impl_mod tdec.ptype_name.Asttypes.txt)
            (exp_of_label_decls ~extensible impl_mod label_decls) :: vbs
        | _ -> (* Nope *) [])
      | Some core_type ->
        if extensible then
          Printf.eprintf "Used ppp_extensible on a primitive type but only \
                          records are extensible.\n%!" ;
        value_binding_of_expr
          (name_of_ppp impl_mod tdec.ptype_name.Asttypes.txt)
          (ppp_exp_of_core_type impl_mod core_type) :: vbs
    ) [] impl_mods

let map_structure mapper str =
  let strs = Ast_mapper.default_mapper.Ast_mapper.structure mapper str in
  (* Try to emit the new ppp definitions just after the corresponding type
     definition: *)
  List.fold_left (fun strs' str ->
    match str.pstr_desc with
    | Pstr_type (rec_flag, type_decls) ->
      let vb_to_emit = ref [] in
      let new_type_decls =
        List.map (fun type_decl ->
          let tdec, vbs = ppps_of_type_declaration type_decl in
          vb_to_emit := List.rev_append vbs !vb_to_emit ;
          tdec
        ) type_decls in
      let strs = Str.type_ rec_flag new_type_decls :: strs' in
      if !vb_to_emit = [] then strs else
        (Str.value Asttypes.Recursive !vb_to_emit) :: strs
    | _ ->
      str::strs'
  ) [] strs |> List.rev

let ppp_mapper _argv =
  { Ast_mapper.default_mapper with
    Ast_mapper.structure = map_structure }

let () =
  Ast_mapper.register "ppx_ppp" ppp_mapper

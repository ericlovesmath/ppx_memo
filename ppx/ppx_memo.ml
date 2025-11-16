open Core
open Ppxlib
open Ast_builder.Default

let memo_attribute =
  Attribute.declare "memo" Attribute.Context.value_binding Ast_pattern.(pstr nil) ()
;;

let memo_recursive_attribute =
  Attribute.declare
    "memo.rec"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)
;;

(** Deconstructs function into list of [args] and [body] *)
let collect_args_and_body expr =
  match expr.pexp_desc with
  | Pexp_function (params, _, Pfunction_body body) ->
    let patterns =
      List.map params ~f:(fun param ->
        match param.pparam_desc with
        | Pparam_val (_, _, pat) -> pat
        | Pparam_newtype _ ->
          Location.raise_errorf
            ~loc:param.pparam_loc
            "Memoization with `newtype` parameters is not supported.")
    in
    patterns, body
  | Pexp_function (_, _, Pfunction_cases (_, fb_loc, _)) ->
    Location.raise_errorf
      ~loc:fb_loc
      "Memoization with the `function` keyword is not supported. Use `fun x -> match x \
       with ...` instead."
  | _ -> Location.raise_errorf ~loc:expr.pexp_loc "Only functions can be memoized"
;;

(** TODO: Handle other cases *)
let expr_of_pat ~loc pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> evar ~loc txt
  | Ppat_any ->
    Location.raise_errorf
      ~loc:pat.ppat_loc
      "Cannot memoize functions with `_` in arguments"
  | _ ->
    Location.raise_errorf
      ~loc:pat.ppat_loc
      "Memoization is only supported for simple variable patterns"
;;

let expand_memo ~loc expr =
  let patterns, body = collect_args_and_body expr in
  match patterns with
  | [] -> Location.raise_errorf ~loc:body.pexp_loc "Cannot memoize constant value"
  | [ _ ] -> [%expr Core.Memo.general [%e expr]]
  | _ ->
    let tuple_pat = ppat_tuple ~loc patterns in
    let arg_exprs = List.map ~f:(expr_of_pat ~loc) patterns in
    let tuple_expr = pexp_tuple ~loc arg_exprs in
    let outer_fun =
      List.fold_right
        patterns
        ~f:(fun p acc -> [%expr fun [%p p] -> [%e acc]])
        ~init:[%expr memoized_fun [%e tuple_expr]]
    in
    [%expr
      let memoized_fun = Core.Memo.general (fun [%p tuple_pat] -> [%e body]) in
      [%e outer_fun]]
;;

let get_function_name_from_pattern pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ ->
    Location.raise_errorf
      ~loc:pat.ppat_loc
      "Memoized recursive function must be simple var binding (`let rec f = ...`)."
;;

let expand_recursive_memo ~loc ~hashable_expr ~function_name expr =
  let patterns, body = collect_args_and_body expr in
  let f_pat = pvar ~loc function_name in
  match patterns with
  | [] -> Location.raise_errorf ~loc:body.pexp_loc "Cannot memoize a recursive constant"
  | [ single_pat ] ->
    let untied_fun = [%expr fun [%p f_pat] [%p single_pat] -> [%e body]] in
    [%expr Core.Memo.recursive ~hashable:[%e hashable_expr] [%e untied_fun]]
  | _ ->
    let tuple_pat = ppat_tuple ~loc patterns in
    let arg_exprs = List.map ~f:(expr_of_pat ~loc) patterns in
    let tuple_expr = pexp_tuple ~loc arg_exprs in
    let untied_fun = [%expr fun [%p f_pat] [%p tuple_pat] -> [%e body]] in
    let outer_fun =
      List.fold_right
        patterns
        ~f:(fun p acc -> [%expr fun [%p p] -> [%e acc]])
        ~init:[%expr memoized_fun [%e tuple_expr]]
    in
    [%expr
      let memoized_fun =
        Core.Memo.recursive ~hashable:[%e hashable_expr] [%e untied_fun]
      in
      [%e outer_fun]]
;;

let transform_value_binding ~rec_flag vb =
  (* TODO: Is there an automatic way to filter / find this *)
  (* Update this helper to remove both kinds of attributes *)
  let remove_memo_attributes attrs =
    List.filter
      ~f:(fun attr ->
        let name = attr.attr_name.txt in
        String.( <> ) name "memo" && String.( <> ) name "memo.rec")
      attrs
  in
  let general_attr = Attribute.get memo_attribute vb in
  let recursive_attr = Attribute.get memo_recursive_attribute vb in
  match rec_flag, general_attr, recursive_attr with
  | Nonrecursive, Some (), None ->
    { vb with
      pvb_expr = expand_memo ~loc:vb.pvb_loc vb.pvb_expr
    ; pvb_attributes = remove_memo_attributes vb.pvb_attributes
    }
  | Recursive, None, Some hashable_expr ->
    let function_name = get_function_name_from_pattern vb.pvb_pat in
    { vb with
      pvb_expr =
        expand_recursive_memo ~loc:vb.pvb_loc ~hashable_expr ~function_name vb.pvb_expr
    ; pvb_attributes = remove_memo_attributes vb.pvb_attributes
    }
  | Recursive, Some (), _ ->
    Location.raise_errorf
      ~loc:vb.pvb_loc
      "Attribute [@@memo] cannot be used on a recursive function. Use [@@memo.rec \
       <hashable>] instead."
  | Nonrecursive, _, Some _ ->
    Location.raise_errorf
      ~loc:vb.pvb_loc
      "Attribute [@@memo.rec] can only be used on a `let rec` binding."
  | Recursive, None, None | Nonrecursive, None, None -> vb
;;

(* TODO: Not a huge fan of this object method *)
(** Apply [memo] to all [let] bindings *)
let memo_mapper =
  object
    inherit Ast_traverse.map as super

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_value (rec_flag, vbs) ->
        let transformed_vbs = List.map ~f:(transform_value_binding ~rec_flag) vbs in
        { item with pstr_desc = Pstr_value (Nonrecursive, transformed_vbs) }
      | _ -> super#structure_item item
  end
;;

let () = Driver.register_transformation "memo" ~impl:memo_mapper#structure

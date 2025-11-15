open Core
open Ppxlib
open Ast_builder.Default

let has_attribute name attrs =
  List.exists ~f:(fun attr -> String.equal attr.attr_name.txt name) attrs
;;

(* TODO: Is there an automatic way to filter this *)
let remove_memo_attributes attrs =
  List.filter
    ~f:(fun attr ->
      let name = attr.attr_name.txt in
      not (String.equal name "memo"))
    attrs
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
    (* This handles `function | p1 -> e1 | ...`. We disallow it for simplicity. *)
    Location.raise_errorf
      ~loc:fb_loc
      "Memoization with the `function` keyword is not supported. Use `fun x -> match x \
       with ...` instead."
  | _ ->
    (* This is the base case for a value that is not a function. *)
    [], expr
;;

(** TODO: Generate fresh variable names *)
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
  | [] | [ _ ] -> [%expr Core.Memo.general [%e expr]]
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

let memo_attribute =
  Attribute.declare "memo" Attribute.Context.value_binding Ast_pattern.(pstr nil) ()
;;

let transform_value_binding vb =
  match Attribute.get memo_attribute vb with
  | None -> vb
  | Some () ->
    { vb with
      pvb_expr = expand_memo ~loc:vb.pvb_loc vb.pvb_expr
    ; pvb_attributes = remove_memo_attributes vb.pvb_attributes
    }
;;

(* TODO: Not a huge fan of this object method *)
(** Apply [memo] to all [let] bindings *)
let memo_mapper =
  object
    inherit Ast_traverse.map as super

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_value (rec_flag, vbs) ->
        let transformed_vbs = List.map ~f:transform_value_binding vbs in
        { item with pstr_desc = Pstr_value (rec_flag, transformed_vbs) }
      | _ -> super#structure_item item
  end
;;

let () = Driver.register_transformation "memo" ~impl:memo_mapper#structure

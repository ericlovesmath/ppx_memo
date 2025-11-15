open Core
open Ppxlib
open Ast_builder.Default

let has_attribute name attrs =
  List.exists ~f:(fun attr -> String.equal attr.attr_name.txt name) attrs

let remove_memo_attributes attrs =
  List.filter
    ~f:(fun attr ->
      let name = attr.attr_name.txt in
      not (String.equal name "memo"))
    attrs

let transform_value_binding vb =
  let loc = vb.pvb_loc in
  let attrs = vb.pvb_attributes in

  match has_attribute "memo" attrs with
  | false -> vb
  | true ->
      (* Handle [@memo] *)
      let memo_expr =
        pexp_apply ~loc
          (evar ~loc "Core.Memo.general")
          [ (Nolabel, vb.pvb_expr) ]
      in
      {
        vb with
        pvb_expr = memo_expr;
        pvb_attributes = remove_memo_attributes attrs;
      }

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

let () = Driver.register_transformation "memo" ~impl:memo_mapper#structure

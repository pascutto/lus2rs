open Ast_lustre
open Ast_types_lustre

module Epsilon = struct
  let consts = Hashtbl.create 5
  let find = Hashtbl.find consts
  let add = Hashtbl.add consts
end

let rec mk_expr e loc =
  {
    pexpr_desc = e;
    pexpr_loc = loc;
  }

let rec transform_expr e =
  { e with pexpr_desc = transform_expr_descr e.pexpr_loc e.pexpr_desc }

and transform_expr_descr loc expr = match expr with
  | LSE_const c -> expr
  | LSE_ident id -> begin try Epsilon.find id
    with Not_found -> expr
  end
  | LSE_binop (op, e1, e2) ->
    LSE_binop (op, transform_expr e1, transform_expr e2)
  | LSE_unop (op, e) -> LSE_unop (op, transform_expr e)
  | LSE_app (id, el, r) ->
    LSE_app (id, List.map transform_expr el, transform_expr r)
  | LSE_arrow (e1, e2) -> LSE_merge(
      mk_expr (LSE_fby (
        (mk_expr (LSE_const(Cbool true)) e1.pexpr_loc),
        (mk_expr (LSE_const(Cbool false)) e2.pexpr_loc))) loc,
      [(Cbool true, transform_expr e1); (Cbool false, transform_expr e2)]
    )
  | LSE_fby (e1, e2) -> LSE_fby (transform_expr e1, transform_expr e2)
  | LSE_pre e -> LSE_pre (transform_expr e)
  | LSE_current e -> LSE_current (transform_expr e)
  | LSE_when (e1, e2, id) -> LSE_when (transform_expr e1, e2, id)
  | LSE_merge (e, ml) ->
    LSE_merge (transform_expr e,
               List.map
                 (fun (c, e) -> (c, transform_expr e)) ml)
  | LSE_tuple (el) -> LSE_tuple (List.map transform_expr el)
  | LSE_if (e1, e2, e3) ->
    let e1 = transform_expr e1 in
    let e2 = transform_expr e2 in
    let e3 = transform_expr e3 in
    LSE_merge (e1,
               [ ((Cbool true), e2);
                 ((Cbool false), e3)])

let transform_equation eq = { eq with lseq_expr = transform_expr eq.lseq_expr }

let transform_node n =
  { n with lsn_eqs = List.map transform_equation n.lsn_eqs }

let transform_constant c =
  Epsilon.add c.lsc_name c.lsc_desc.pexpr_desc;
  c

let transform_element = function
  | LS_Node n -> LS_Node (transform_node n)
  | LS_Constant c -> LS_Constant (transform_constant c)

let transform p = List.filter
    (function LS_Constant _ -> false | _ -> true)
    (List.map transform_element p)

(*
########
Copyright © 2017

This file is part of lus2rs.
lus2rs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Clément PASCUTTO <clement.pascutto@ens.fr
########
*)

open Ast_clocked_lustre
open Ast_obj

module M = Map.Make(Ident)

let rec const_of_expr e =
  match e.cexpr_desc with
  | CE_const c -> c
  | _ -> assert false

let base_clock_of_clock = function
  | [c] -> c
  | e -> assert false

  let base_typ_of_typ = function
    | [c] -> c
    | e -> assert false

let rec control ck instr = match ck with
  | Base -> instr
  | Clk(clk, id, cond) -> control clk (OS_Case(id, [const_of_expr cond, instr]))

let rec join s1 s2 = match s1, s2 with
  | OS_Case(id1, l1), OS_Case(id2, l2)
    when id1 = id2 && List.map fst l1 = List.map fst l2 ->
    OS_Case(id1, (List.map2 (fun (c1,s1) (c2,s2) -> (c1, join s1 s2)) l1 l2))
  | _, _ -> OS_Sequence (s1, s2)

let rec join_list = function
  | OS_Sequence (s1, s2) -> join s1 (join_list s2)
  | s -> s

and trans_expr env e =
  let m, si, j, d, s = env in match e.cexpr_desc with
  | CE_const c -> OE_Const c
  | CE_ident id -> if M.mem id m then OE_State id
    else if M.mem id d then OE_Ident id
    else assert false
  | CE_unop (op, e) -> OE_Unop (op, trans_expr env e)
  | CE_binop (op, e1, e2) -> OE_Binop (op, trans_expr env e1, trans_expr env e2)
  | CE_when (e, cond, id) -> trans_expr env e
  | CE_current e -> trans_expr env e
  | CE_if (_, _, _) -> assert false
  | _ -> assert false

and trans_aux env x expr = match expr.cexpr_desc with
  | CE_merge (id, ml) ->
    OS_Case (id,
             List.map (fun (c, e) -> const_of_expr c, trans_aux env x e) ml)
  | _ -> OS_Var_assign (x, trans_expr env expr)

and trans_eq env eq =
  let m, si, j, d, s = env in
  let expr = eq.ceq_expr in
  let patt = eq.ceq_patt in
  match expr.cexpr_desc with
  | CE_fby (e1, e2) -> begin
      let x = List.hd patt.cpatt_desc in
      let c = trans_expr env e2 in
      let newm = M.add x (base_typ_of_typ expr.cexpr_type) m in
      let newsi = OS_Sequence (
          OS_State_assign
            (x, OE_Const(const_of_expr e1)),
          si) in
      let news = OS_Sequence (
          control
            (base_clock_of_clock expr.cexpr_clock)
            (OS_State_assign (x, c)),
          s) in
      newm, newsi, j, d, news
    end
  | CE_app (f, el) -> assert false
  | _ ->
    let x = List.hd patt.cpatt_desc in
    let news = control
        (base_clock_of_clock expr.cexpr_clock)
        (trans_aux env x expr) in
    m, si, j, d, news

and trans_eq_list env = List.fold_left trans_eq env

and trans_node n =
  let initr = List.fold_left
      (fun r (id, t, clk) -> M.add id t r) M.empty n.cn_local in
  let m, si, j, d, s =
    trans_eq_list (M.empty, OS_Skip, M.empty, initr, OS_Skip) n.cn_equs in
  {
    oc_name = n.cn_name;
    oc_mem = M.bindings m;
    oc_instances = M.bindings j;
    oc_reset = si;
    oc_step =
      List.map (fun (id, t, clk) -> id, t) n.cn_input,
      List.map (fun (id, t, clk) -> id, t) n.cn_output,
      M.bindings d,
      join_list s;
  }

let translate =
  List.map trans_node

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

let i = ref 0
let new_obj() = incr i; !i

let rec const_of_expr e =
  match e.cexpr_desc with
  | CE_const c -> c
  | _ -> assert false

let rec const_or_ident e =
  match e.cexpr_desc with
  | CE_const c -> true, OE_Const c
  | CE_ident id -> false, OE_Ident id
  | _ -> assert false

let base_clock_of_clock = function
  | [c] -> c
  | e -> assert false

  let base_typ_of_typ = function
    | [c] -> c
    | e -> assert false

let rec control ck instr = match ck with
  | Base -> instr
  | Clk(clk, id, cond) -> control clk (OS_Case(OE_Ident id, [cond, instr]))

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
  | e -> Format.eprintf "%a" Printer_clocked_ast.print_exp_desc e;
    assert false

and trans_aux env x expr = match expr.cexpr_desc with
  | CE_merge (e, ml) ->
    OS_Case ((trans_expr env e),
             List.map (fun (c, e) -> c, trans_aux env x e) ml)
  | CE_arrow (e1, e2) -> assert false
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
      let const, coe1 = const_or_ident e1 in
      let newsi = if const
        then OS_Sequence (si, OS_State_assign (x, coe1))
        else OS_Sequence (si, OS_Var_assign (x, coe1)) in
      let news = OS_Sequence (s,
          control
            (base_clock_of_clock expr.cexpr_clock)
            (OS_State_assign (x, c))) in
      newm, newsi, j, d, news
    end
  | CE_pre e -> begin
      let x = List.hd patt.cpatt_desc in
      let c = trans_expr env e in
      let newm = M.add x (base_typ_of_typ expr.cexpr_type) m in
      let newsi = si in
      let news = OS_Sequence (s,
                              control
                                (base_clock_of_clock expr.cexpr_clock)
                                (OS_State_assign (x, c))) in
      newm, newsi, j, d, news
    end
  | CE_app (f, el) ->
    let c = List.map (trans_expr env) el in
    let o = new_obj() in
    let newsi = OS_Sequence (si, OS_Reset(o)) in
    let newj = (o, f) :: j in
    let news = OS_Sequence (s,
                            control
                              (base_clock_of_clock expr.cexpr_clock)
                              (OS_Step (patt.cpatt_desc, o, c)))
    in
    m, newsi, newj, d, news
  | _ ->
    let x = List.hd patt.cpatt_desc in
    let news = OS_Sequence (s,
        control
          (base_clock_of_clock expr.cexpr_clock)
          (trans_aux env x expr)) in
    m, si, j, d, news

and trans_eq_list env = List.fold_left trans_eq env

and trans_node n =
  let initr = List.fold_left
      (fun r (id, t, clk) -> M.add id t r)
      M.empty
      (n.cn_local@n.cn_input@n.cn_output) in
  let m, si, j, d, s =
    trans_eq_list (M.empty, OS_Skip, [], initr, OS_Skip) n.cn_equs in
  {
    oc_name = n.cn_name;
    oc_mem = M.bindings m;
    oc_instances = j;
    oc_reset = si;
    oc_step =
      List.map (fun (id, t, clk) -> id, t) n.cn_input,
      List.map (fun (id, t, clk) -> id, t) n.cn_output,
      M.bindings d,
      join_list s;
  }

let translate =
  List.map trans_node

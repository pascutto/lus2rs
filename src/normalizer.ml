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

let new_local =
  let cpt = ref 0 in
  fun () -> incr cpt; Ident.make ("aux'"^(string_of_int !cpt)) Ident.Stream

let rec combine2 l1 l2 l3 =
  match l1, l2, l3 with
  | [], [], [] -> []
  | h1::t1, h2::t2, h3::t3 -> (h1, h2, h3) :: combine2 t1 t2 t3
  | _ -> assert false

let new_pat ({ cexpr_type = ty; cexpr_clock = clk; cexpr_loc = loc } as e) =
  match ty, clk with
  | [t], [c] ->
    let x = new_local() in
    let decl = [x,t,c] in
    let patt = {
      cpatt_desc = [x]; cpatt_type = ty; cpatt_clock = clk; cpatt_loc = loc
    } in
    let expr = { e with cexpr_desc = CE_ident x } in
    decl, patt, expr
  | lt, lc ->
    let lx = List.map (fun _ -> new_local()) lt in
    let decl = combine2 lx lt lc in
    let patt = {
      cpatt_desc = lx; cpatt_type = ty; cpatt_clock = clk; cpatt_loc = loc
    } in
    let le = List.map
      (fun (x, t, c) ->
        {
          cexpr_desc = CE_ident x;
          cexpr_type = [t];
          cexpr_clock = [c];
          cexpr_loc = loc
        }
      ) decl
    in
    decl, patt, { e with cexpr_desc = CE_tuple le }

let rec normalize ctx e =
  match e.cexpr_desc with
  | CE_const _ | CE_ident _ -> ctx, e

  | CE_unop(op,e1) ->
    let ctx, e1' = normalize ctx e1 in
    ctx, { e with cexpr_desc = CE_unop(op,e1') }

  | CE_binop(op,e1,e2) ->
    let ctx, e1' = normalize ctx e1 in
    let ctx, e2' = normalize ctx e2 in
    ctx, {e with cexpr_desc = CE_binop(op, e1', e2')}

  | CE_app(n,le) ->
    let (new_vars,new_eqs), le' = normalize_list ctx le in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { ceq_patt = x_patt;
        ceq_expr = { e with cexpr_desc = CE_app(n,le') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_prim(n,le) ->
    let (new_vars,new_eqs), le' = normalize_list ctx le in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { ceq_patt = x_patt;
        ceq_expr = { e with cexpr_desc = CE_prim(n,le') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_tuple l ->
    let ctx, l' = normalize_list ctx l in
    ctx, { e with cexpr_desc = CE_tuple l'}

  | CE_pre e1 ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { ceq_patt = x_patt;
        ceq_expr = { e with cexpr_desc = CE_pre e1' }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_current e1 ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { ceq_patt = x_patt;
        ceq_expr = { e with cexpr_desc = CE_current e1' }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_when (e1, cond, clk) ->
    let ctx, e1' = normalize ctx e1 in
    ctx, {e with cexpr_desc = CE_when(e1', cond, clk)}

  | CE_merge (e1, mat) ->
    let ctx, e1' = normalize ctx e1 in
    let (new_vars,new_eqs), mat' = normalize_matching ctx mat in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq = {
      ceq_patt = x_patt;
      ceq_expr = { e with cexpr_desc = CE_merge (e1', mat') }; }
  in
  (x_decl@new_vars, x_eq::new_eqs), x_expr

  | CE_fby(c,e1) ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { ceq_patt = x_patt;
        ceq_expr = { e with cexpr_desc = CE_fby(c, e1') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

and normalize_list ctx l =
  let ctx, l =
    List.fold_left
      (fun (ctx,l) e ->
	let ctx, e' = normalize ctx e in
	ctx, e'::l ) (ctx,[]) l
  in ctx, List.rev l

and normalize_matching ctx l =
  let ctx, l =
    List.fold_left
      (fun (ctx, l) (c, e) ->
         let c, (ctx, e') = c, normalize ctx e in
         ctx, (c, e')::l ) (ctx, []) l
  in ctx, List.rev l

let normalize_equation node e =
  let (locals, new_eqs), e' = normalize ([],[]) e.ceq_expr in
  { node with
    cn_local = locals@node.cn_local;
    cn_equs = { e with ceq_expr = e' } :: (List.rev new_eqs) @ node.cn_equs }

let normalize_node = function n ->
  let n =
    List.fold_left normalize_equation { n with cn_equs = [] } n.cn_equs
  in
  { n with cn_equs = List.rev n.cn_equs }

let normalize_file = List.map normalize_node

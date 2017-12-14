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

(* FIXME : Add correct subtyping *)

open Ast_types_lustre
open Ast_typed_lustre
open Ast_clocked_lustre
open Format

module S = Set.Make(Ident)
module M = Map.Make(Ident)

type error =
  | ExpectedSub of clock * clock
  | ExpectedBase of clock
  | ExpectedSame

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error loc e = raise (Error (loc, e))

let print_const fmt = function
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let rec print_base_clock fmt = function
  | Base -> fprintf fmt "base"
  | Clk(clk, id, cons) -> begin match cons.cexpr_desc with
      | CE_const(c) -> fprintf fmt "(%a on %a(@[%a@]))"
                         print_base_clock clk print_const c Ident.print id
      | _ -> assert false
    end

let print_clock fmt = function
  | ([]) -> fprintf fmt "empty tuple"
  | [t] -> print_base_clock fmt t
  | (t::tl) ->
    fprintf fmt "(";
    print_base_clock fmt t;
    List.iter (fun t -> fprintf fmt " * %a" print_base_clock t) tl;
    fprintf fmt ")"

let report fmt = function
  | ExpectedSub (c1, c2) ->
    fprintf fmt
    "This expression has clock %a but is expected be a subclock of %a"
    print_clock c1 print_clock c2
  | ExpectedBase clk ->
    fprintf fmt
      "This expression has clock %a but is expected to have a type simple clock"
      print_clock clk
  | ExpectedSame ->
    fprintf fmt
      "These expressions are supposed to be on the same clock."


module Delta = struct
  let nodes = Hashtbl.create 5

  let find n =
    Hashtbl.find nodes n

  let add x clk =
    Hashtbl.replace nodes x (x, clk);
    x
end

type io = Vinput | Vpatt
module Gamma = struct
  type t = (Ident.t * base_clock * io) M.t

  let empty = M.empty

  let add loc env x c io =
    M.add x (x, c, io) env

  let adds loc io =
    List.fold_left (fun env (x, _) -> add loc env x Base io)
      (* TODO Clocks of new variables *)

  let find loc env x = M.find x env
end

module Epsilon = struct
  let consts = Hashtbl.create 5

  let find loc = Hashtbl.find consts

  let add loc x clk = Hashtbl.add consts x (x, clk)
end

let base_clock_of_clock loc = function
  | [c] -> c
  | e -> error loc (ExpectedBase e)

let rec sub_base clk1 clk2 = match clk1 with
  | c when c = clk2 -> true
  | Clk(clk, id, cond) -> clk = clk2 || sub_base clk clk2
  | _ -> false

let sub actual_clk expected_clk =
  try
    List.fold_left2
      (fun well_c ac_c ex_c ->
         let well_c' = sub_base ac_c ex_c in
         (well_c && well_c'))
      true actual_clk expected_clk
  with Invalid_argument _ -> false

let rec clock_expr env e =
  let desc, clk = clock_expr_desc env e.texpr_loc e.texpr_desc in
  {
    cexpr_desc = desc;
    cexpr_type = e.texpr_type;
    cexpr_clock = clk;
    cexpr_loc = e.texpr_loc;
  }

and clock_expr_desc env loc = function
  | TE_const c -> CE_const c , [Base]

  | TE_ident x -> begin
    try
      let x, clk, _ = Gamma.find loc env x in
      CE_ident x , [clk]
    with _ ->
      let x, clk = Epsilon.find loc x in
      CE_ident x , [clk]
    end

  | TE_unop (op, e) ->
    let ce = clock_expr env e in
    CE_unop(op, ce) , ce.cexpr_clock

  | TE_binop (op, e1, e2) ->
    let ce1 = clock_expr env e1 in
    let clk1 = ce1.cexpr_clock in
    let ce2 = clock_expr env e2 in
    let clk2 = ce2.cexpr_clock in
    if sub clk1 clk2 then CE_binop (op, ce1, ce2), clk1
    else if sub clk2 clk1 then CE_binop (op, ce1, ce2), clk2
    else error loc (ExpectedSub(clk2, clk1))

  | TE_if (e1, e2, e3) ->
    let ce1 = clock_expr env e1 in
    let clk1 = ce1.cexpr_clock in
    let ce2 = clock_expr env e2 in
    let clk2 = ce2.cexpr_clock in
    let ce3 = clock_expr env e3 in
    let clk3 = ce3.cexpr_clock in
    let clk =
    if sub clk1 clk2 then begin
      if sub clk3 clk1 then clk3
      else if sub clk1 clk3 then clk1
      else error loc (ExpectedSub(clk3, clk1)) end
    else if sub clk2 clk1 then begin
      if sub clk3 clk2 then clk3
      else if sub clk2 clk3 then clk2
      else error loc (ExpectedSub(clk3, clk2)) end
    else error loc (ExpectedSub(clk2, clk1))
    in CE_if (ce1, ce1, ce2), clk

  | TE_app (f, el) ->
    let cel = List.map (clock_expr env) el in
    let celclk = List.flatten (List.map (fun x -> x.cexpr_clock) cel) in
    let _, (clki, clko) = Delta.find f in
    if sub celclk clki then
      CE_app (f, cel), clko
    else error loc ExpectedSame

  | TE_arrow (e1, e2) ->
    let ce1 = clock_expr env e1 in
    let clk1 = ce1.cexpr_clock in
    let ce2 = clock_expr env e2 in
    let clk2 = ce2.cexpr_clock in
    if sub clk1 clk2 then CE_arrow (ce1, ce2), clk1
    else if sub clk2 clk1 then CE_arrow (ce1, ce2), clk2
    else error loc (ExpectedSub(clk2, clk1))

  | TE_fby (e1, e2) ->
    let ce1 = clock_expr env e1 in
    let clk1 = ce1.cexpr_clock in
    let ce2 = clock_expr env e2 in
    let clk2 = ce2.cexpr_clock in
    if sub clk1 clk2 then CE_fby (ce1, ce2), clk1
    else if sub clk2 clk1 then CE_fby (ce1, ce2), clk2
    else error loc (ExpectedSub(clk2, clk1))

  | TE_pre e ->
    let ce = clock_expr env e in
    CE_pre(ce), ce.cexpr_clock

  | TE_current e ->
    let ce = clock_expr env e in
    CE_current ce, [Base]

  | TE_tuple el ->
    let tel = List.map (clock_expr env) el in
    CE_tuple tel,
    (List.map (fun e -> base_clock_of_clock e.cexpr_loc e.cexpr_clock) tel)

  | TE_when(e, cond, id) ->
    let ce = clock_expr env e in
    let ceclk = base_clock_of_clock ce.cexpr_loc ce.cexpr_clock in
    let ccond = clock_expr env cond in
    let idclk =
      try
        let _, clk, _ = Gamma.find loc env id in clk
      with _ -> let _, clk = Epsilon.find loc id in clk in
    if sub_base ceclk idclk
    then CE_when(ce, ccond, id), [Clk(ceclk, id, ccond)]
    else if sub_base idclk ceclk
    then CE_when(ce, ccond, id), [Clk(ceclk, id, ccond)]
    else error loc (ExpectedSub([idclk], [ceclk]))

  | TE_merge(e, mat) -> assert false (* TODO *)
  | _ -> assert false

and clock_args env loc params_clk el =
  let cel = List.map (clock_expr env) el in
  let actual_clocks =
    List.rev
      begin
        List.fold_left
          (fun res te -> List.rev_append te.cexpr_clock res)
          [] cel
      end
  in
  let well_clocked =
    sub actual_clocks params_clk
  in
  if well_clocked then cel
  else error loc (ExpectedSub (actual_clocks, params_clk));


and expected_clock env e clk =
  let ce = clock_expr env e in
  let cec = ce.cexpr_clock in
  if cec = clk then ce
  else error e.texpr_loc (ExpectedSub (cec, clk))

and expected_base_clock env e =
  let ce = clock_expr env e in
  match ce.cexpr_clock with
  | [_] -> ce
  |  _ ->  error e.texpr_loc (ExpectedBase (ce.cexpr_clock))

let rec clock_patt env p =
  let clks = clock_patt_desc env p.tpatt_loc p.tpatt_desc in
  {
    cpatt_desc = p.tpatt_desc;
    cpatt_type = p.tpatt_type;
    cpatt_clock = clks;
    cpatt_loc = p.tpatt_loc;
  }

and clock_patt_desc env loc patt =
  List.map
    (fun x ->
       match Gamma.find loc env x with
       | x, clk, Vpatt -> clk
       | _  -> assert false (* TODO What is this ? *)
    ) patt

let clock_equation env eq =
  let patt = clock_patt env eq.teq_patt in
  let expr = clock_expr env eq.teq_expr in
  let well_clocked = sub expr.cexpr_clock patt.cpatt_clock in
  if well_clocked then
    { ceq_patt = patt; ceq_expr = expr; }
  else
    error
      eq.teq_expr.texpr_loc (ExpectedSub (expr.cexpr_clock, patt.cpatt_clock))

let add_vars_of_patt loc s {ceq_patt = {cpatt_desc = p}} =
  let add x s = S.add x s in
  List.fold_left (fun s x -> add x s) s p

let tclk_to_clk = function
  | TBase -> Base
  | TClk(clk, cond) -> begin match cond.texpr_desc with
      | TE_const(c) -> let cond = {
          cexpr_desc = CE_const(c);
          cexpr_type = cond.texpr_type;
          cexpr_clock = [Base];
          cexpr_loc = cond.texpr_loc;
        } in
        Clk(Base, clk, cond)
      | _ -> assert false
  end

let clock_node n =
  let clock_decl = (fun (x, typ, tclk) -> (x, tclk_to_clk tclk)) in
  let out_clk = List.map clock_decl n.tn_output in
  let loc_clk = List.map clock_decl n.tn_local in
  let in_clk = List.map clock_decl n.tn_input in
  let env = Gamma.adds n.tn_loc Vpatt Gamma.empty (out_clk@loc_clk) in
  let env = Gamma.adds n.tn_loc Vinput env in_clk in
  let equs = List.map (clock_equation env) n.tn_equs in

  let input =
    List.map
      (fun (x, typ, clk) -> let x', clk, _ = Gamma.find n.tn_loc env x in (x', typ, clk))
      n.tn_input
  in
  let output =
    List.map
      (fun (x, typ, clk) -> let x', clk, _ = Gamma.find n.tn_loc env x in (x', typ, clk))
      n.tn_output
  in
  let local =
    List.map
      (fun (x, typ, clk) -> let x', clk, _ = Gamma.find n.tn_loc env x in (x', typ, clk))
      n.tn_local
  in
  let c_in = List.map (fun (_, _, clk) -> clk ) input in
  let c_out = List.map (fun (_, _, clk) -> clk) output in
  let name = Delta.add n.tn_name (c_in, c_out) in
  let node =
    { cn_name = name;
      cn_input = input;
      cn_output = output;
      cn_local = local;
      cn_equs = equs;
      cn_loc = n.tn_loc; }
  in
  node

let clock_constant c =
  let cexpr = clock_expr Gamma.empty c.tc_desc in
  Epsilon.add c.tc_desc.texpr_loc c.tc_name (List.hd cexpr.cexpr_clock);
  {
    cc_name = c.tc_name;
    cc_desc = cexpr;
    cc_type = cexpr.cexpr_type;
    cc_clock = [Base];
  }

let clock_element = function
  | T_Node n -> C_Node(clock_node n)
  | T_Constant c -> C_Constant(clock_constant c)

let clock_program ndl  =
  List.map clock_element ndl

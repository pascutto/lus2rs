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

open Lustre_ast_types
open Lustre_typed_ast
open Lustre_clocked_ast
open Format

module S = Set.Make(Ident)
module M = Map.Make(Ident)

type error =
  | ExpectedClock of clock * clock
  | ExpectedBase of clock

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error loc e = raise (Error (loc, e))

let print_const fmt = function
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let print_base_clock fmt = function
  | Base -> fprintf fmt "_base_"
  | Clk(id, cons) -> fprintf fmt "%a(@[%a@])" Ident.print id print_const cons

let print_clock fmt = function
  | ([]) -> fprintf fmt "empty tuple"
  | [t] -> print_base_clock fmt t
  | (t::tl) ->
    fprintf fmt "(";
    print_base_clock fmt t;
    List.iter (fun t -> fprintf fmt " * %a" print_base_clock t) tl;
    fprintf fmt ")"

let report fmt = function
  | ExpectedClock (c1, c2) ->
    fprintf fmt
    "this expression has clock %a but is expected to have clock %a"
    print_clock c1 print_clock c2
  | ExpectedBase clk ->
    fprintf fmt
      "this expression has clock %a but is expected to have a type simple clock"
      print_clock clk

module Delta = struct

  let nodes = Hashtbl.create 97

  (* TODO Depreciated : remove this *)
  let is_primitive f = false

  let find n =
    Hashtbl.find nodes n , false

  let add x clk =
    Hashtbl.replace nodes x (x, clk);
    x

  let save () = Hashtbl.fold (fun key (_, clk) env -> (key, clk)::env) nodes []
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

  let patts_vars env =
    M.fold (fun _ (x,_,io) s -> if io = Vpatt then S.add x s else s) env S.empty

end

let base_clock_of_clock loc = function
  | [c] -> c
  | e -> error loc (ExpectedBase e)

let compatible_base actual_clk expected_clk=
  actual_clk = expected_clk

let compatible actual_clk expected_clk =
  try
    List.fold_left2
      (fun well_c ac_c ex_c ->
         let well_c' = compatible_base ac_c ex_c in
         (well_c && well_c'))
      true actual_clk expected_clk
  with Invalid_argument _ -> false

let rec is_constant env e =
  match e.texpr_desc with
  | TE_const _ -> true
  | TE_tuple el -> List.for_all (is_constant env) el
  | _ -> false

let rec const_of_expr e =
  match e.texpr_desc with
  | TE_const c -> [c]
  | TE_tuple el ->
    List.fold_right (fun e acc -> const_of_expr e @ acc) el []
  | _ -> assert false

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

  | TE_ident x ->
    let x, clk, _ = Gamma.find loc env x in
    CE_ident x , [clk]

  | TE_unop (op, e) ->
    let ce = clock_expr env e in
    CE_unop(op, ce) , ce.cexpr_clock

  | TE_binop (op, e1, e2) ->
    let ce1 = clock_expr env e1 in
    let clk1 = ce1.cexpr_clock in
    let ce2 = clock_expr env e2 in
    let clk2 = ce2.cexpr_clock in
    begin match clk1, clk2 with
      | [c1], [c2] when c1 = c2 -> CE_binop (op, ce1, ce2), clk1
      | _ -> assert false (* TODO Raise error *)
    end

  | TE_if (e1, e2, e3) ->
    let ce1 = clock_expr env e1 in
    let ce2 = clock_expr env e2 in
    let ce3 = clock_expr env e3 in
    if ce1.cexpr_clock = ce2.cexpr_clock && ce2.cexpr_clock = ce3.cexpr_clock
    then
      let clk = ce2.cexpr_clock in
      CE_if(ce1, ce2, ce3), clk
    else assert false  (* TODO raise error *)

  | TE_app (f, el) -> assert false (* TODO *)

  | TE_arrow (e1, e2) -> assert false (* TODO *)

  | TE_fby (e1, e2) -> assert false (* TODO *)

  | TE_pre e ->
    let ce = clock_expr env e in
    CE_pre(ce), ce.cexpr_clock

  | TE_current e ->
    let ce = clock_expr env e in
    CE_current ce, [Base]

  | TE_tuple el -> assert false (* TODO *)
    (* not_a_nested_tuple n loc;
    let tel = List.map (type_expr env) el in
    TE_tuple tel,
    (List.map (fun e -> base_clock_of_clock e.texpr_loc e.texpr_type) tel) *)

  | TE_when(e, cond, clk) -> assert false (* TODO *)
  | TE_merge(e, mat) -> assert false (* TODO *)
  | _ -> assert false

and type_args env loc params_clk el =
  let cel = List.map (clock_expr env) el in
  let actual_clocks =
    List.rev
      begin
        List.fold_left
          (fun res te -> List.rev_append te.cexpr_clock res)
          [] cel
      end
  in
  let well_typed =
    compatible actual_clocks params_clk
  in
  if well_typed then cel
  else error loc (ExpectedClock (actual_clocks, params_clk));


and expected_clock env e clk =
  let ce = clock_expr env e in
  let cec = ce.cexpr_clock in
  if cec = clk then ce
  else error e.texpr_loc (ExpectedClock (cec, clk))

and expected_base_typpe env e =
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
  let well_clocked = compatible expr.cexpr_clock patt.cpatt_clock in
  if well_clocked then
    { ceq_patt = patt; ceq_expr = expr; }
  else
    error
      eq.teq_expr.texpr_loc (ExpectedClock (expr.cexpr_clock, patt.cpatt_clock))

let add_vars_of_patt loc s {ceq_patt = {cpatt_desc = p}} =
  let add x s = S.add x s in
  List.fold_left (fun s x -> add x s) s p

let clock_node n =
  let out_clk = List.map (fun (x, typ) -> (x, Base)) n.tn_output in
  let loc_clk = List.map (fun (x, typ) -> (x, Base)) n.tn_local in
  let in_clk = List.map (fun (x, typ) -> (x, Base)) n.tn_local in
  let env = Gamma.adds n.tn_loc Vpatt Gamma.empty (out_clk@loc_clk) in
  let env = Gamma.adds n.tn_loc Vinput env in_clk in
  let equs = List.map (clock_equation env) n.tn_equs in
  let t_in = List.map (fun (_, clk) -> clk) n.tn_input in
  let t_out = List.map (fun (_, clk) -> clk) n.tn_output in
  let name = Delta.add n.tn_name (t_in,t_out) in
  let input =
    List.map
      (fun (x, typ) -> let x', _, _ = Gamma.find n.tn_loc env x in (x', typ, Base))
      n.tn_input
  in
  let output =
    List.map
      (fun (x, typ) -> let x', _, _ = Gamma.find n.tn_loc env x in (x', typ, Base))
      n.tn_output
  in
  let local =
    List.map
      (fun (x, typ) -> let x', _, _ = Gamma.find n.tn_loc env x in (x', typ, Base))
      n.tn_local
  in
  let node =
    { cn_name = name;
      cn_input = input;
      cn_output = output;
      cn_local = local;
      cn_equs = equs;
      cn_loc = n.tn_loc; }
  in
  node

let clock_program ndl  =
  List.map clock_node ndl
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

open Ast_types_lustre
open Ast_typed_lustre
open Ast_clocked_lustre
open Format

module S = Set.Make(Ident)
module M = Map.Make(Ident)

type error =
  | ExpectedSub of clock * clock
  | ExpectedSimple of clock
  | ExpectedSame
  | UnboundVar of Ident.t

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
let error loc e = raise (Error (loc, e))

let print_const fmt = function
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let rec print_base_clock fmt = function
  | Base -> fprintf fmt "base"
  | Clk(clk, id, cons) ->
    fprintf fmt "(%a on %a(@[%a@]))"
      print_base_clock clk print_const cons Ident.print id

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
    "This expression has clock %a but was expected be a subclock of %a."
    print_clock c1 print_clock c2
  | ExpectedSimple clk ->
    fprintf fmt
      "This expression has clock %a but was expected to have a simple clock"
      print_clock clk
  | ExpectedSame ->
    fprintf fmt
      "These expressions are supposed to be on the same clock."
  | UnboundVar id -> fprintf fmt "unbound variable %a in node clock declaration" Ident.print id

type signature =
  | CBase
  | CClk of signature * int * const

let rec signature_from loc (pi, po) =
  let i = ref 0 in
  let fresh() = (incr i; !i) in
  let vars = Hashtbl.create 5 in
  let rec aux = function
    | ident, t, Base -> Hashtbl.add vars ident (fresh()); CBase
    | ident, t, (Clk(clk, id, cond)) ->
      let sclk = aux (ident, t, clk) in
      try
        let sid = Hashtbl.find vars id in CClk(sclk, sid, cond)
      with Not_found -> error loc (UnboundVar id)
  in
  let scki = List.map aux pi in
  let scko = List.map aux po in
  scki, scko

let satisfy_signature pi (signi, signo) =
  let vars = Hashtbl.create 5 in
  let rec aux acc ck sg = match ck, sg with
    | _, CBase -> acc
    | Clk(clk, id, cond), CClk(sclk, sid, scond) when cond = scond -> begin
      if aux acc clk sclk then try
        Hashtbl.find vars sid = id
        with Not_found -> (Hashtbl.replace vars sid id; acc)
      else false
    end
    | _ -> false
  in
  let rec replace = function
    | CBase -> Base
    | CClk(sclk, sid, cond) -> try
      let id = Hashtbl.find vars sid in Clk(replace sclk, id, cond)
    with  | Not_found -> assert false
  in
  if List.fold_left2 aux true pi signi then List.map replace signo
  else assert false

module Delta = struct
  let nodes = Hashtbl.create 5
  let find x clki = satisfy_signature clki (Hashtbl.find nodes x)
  let add x loc (clki, clko) = Hashtbl.replace nodes x (signature_from loc (clki, clko))
end

module Gamma = struct
  type t = base_clock M.t
  let empty = M.empty
  let add env x clk = M.add x clk env
  let adds = List.fold_left (fun env (x, clk) -> add env x clk)
  let find env x = M.find x env
end

let base_clock_of_clock loc = function
  | [c] -> c
  | e -> error loc (ExpectedSimple e)

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

  | TE_ident x ->
    let clk = Gamma.find env x in
    CE_ident x , [clk]

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

  | TE_app (f, el, r) ->
    let cr = clock_expr env r in
    let cel = List.map (clock_expr env) el in
    let celclk = List.flatten (List.map (fun x -> x.cexpr_clock) cel) in
    let clko = Delta.find f celclk in
      CE_app (f, cel, cr), clko

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
    let clk = match base_clock_of_clock ce.cexpr_loc ce.cexpr_clock with
      | Base -> Base
      | Clk(ck, _, _) -> ck
    in
    CE_current ce, [clk]

  | TE_tuple el ->
    let tel = List.map (clock_expr env) el in
    CE_tuple tel,
    (List.map (fun e -> base_clock_of_clock e.cexpr_loc e.cexpr_clock) tel)

  | TE_when(e, cond, id) ->
    let ce = clock_expr env e in
    let ceclk = base_clock_of_clock ce.cexpr_loc ce.cexpr_clock in
    let idclk = Gamma.find env id in
    if sub_base ceclk idclk
    then CE_when(ce, cond, id), [Clk(ceclk, id, cond)]
    else if sub_base idclk ceclk
    then CE_when(ce, cond, id), [Clk(ceclk, id, cond)]
    else error loc (ExpectedSub([idclk], [ceclk]))

  | TE_merge(e, mat) ->
    let ce = clock_expr env e in
    let ck = ce.cexpr_clock in
    let cmat = List.map (fun (c, e) -> c, clock_expr env e) mat in
    let ok =
      (* FIXME ! Issue : ck on C(x) is not defined since x is not an ident *)
      List.for_all
        (fun (c, e) -> sub ck e.cexpr_clock)
        cmat
    in
    if ok
    then CE_merge (ce, cmat), ce.cexpr_clock
    else assert false

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
  |  _ ->  error e.texpr_loc (ExpectedSimple (ce.cexpr_clock))

let rec clock_patt env p =
  let clks = clock_patt_desc env p.tpatt_loc p.tpatt_desc in
  {
    cpatt_desc = p.tpatt_desc;
    cpatt_type = p.tpatt_type;
    cpatt_clock = clks;
    cpatt_loc = p.tpatt_loc;
  }

and clock_patt_desc env loc patt =
  List.map (Gamma.find env) patt

let clock_equation env eq =
  let patt = clock_patt env eq.teq_patt in
  let expr = clock_expr env eq.teq_expr in
  let well_clocked = expr.cexpr_clock = patt.cpatt_clock in
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
  | TClk(clk, cond) -> Clk(Base, clk, cond)

let clock_node n =
  let clock_decl = (fun (x, typ, tclk) -> (x, tclk_to_clk tclk)) in
  let out_clk = List.map clock_decl n.tn_output in
  let loc_clk = List.map clock_decl n.tn_local in
  let in_clk = List.map clock_decl n.tn_input in
  let env = Gamma.adds Gamma.empty (out_clk@loc_clk@in_clk) in
  let equs = List.map (clock_equation env) n.tn_equs in
  let aux = fun (x, typ, clk) -> let clk = Gamma.find env x in (x, typ, clk) in
  let input = List.map aux n.tn_input in
  let output = List.map aux n.tn_output in
  let local = List.map aux n.tn_local in
  Delta.add n.tn_name n.tn_loc (input, output);
  { cn_name = n.tn_name;
    cn_input = input;
    cn_output = output;
    cn_local = local;
    cn_equs = equs;
    cn_loc = n.tn_loc; }

let clock_program ndl  =
  List.map clock_node ndl

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
open Format

type init =
  | I_Zero
  | I_One
  | I_Var of int
  | I_Max of initr * initr

and initr = init ref

type error = Dummy

exception Error of error
let error e = raise (Error(e))

let i = ref 0
let new_var () = incr i; ref(I_Var(!i))

let imax i1 i2 = match !i1, !i2 with
  | I_Zero, _ -> i2
  | _, I_Zero -> i1
  | _, I_One | I_One, _ -> ref I_One
  | _, _ -> ref (I_Max(i1, i2))

let imax_list = List.fold_left imax (ref I_Zero)

module M = Map.Make(Ident)
module Gamma = struct
  type t = initr M.t
  let empty = M.empty
  let add env x it = M.add x it env
  let add_new env x = M.add x (new_var()) env
  let adds env l = List.fold_left (fun env (x, it) -> add env x it) env l
  let adds_zero env l = List.fold_left (fun env x -> add env x (ref I_Zero)) env l
  let adds_new env l = List.fold_left (fun env x -> add_new env x) env l
  let find env x = M.find x env
end

let rec force_zero i = match !i with
  | I_Zero -> ()
  | I_Var _ -> i := I_Zero
  | I_Max(i1, i2) -> force_zero i1; force_zero i2
  | I_One -> error Dummy


let rec force_sub t1 t2 =
  let rec force_zero_occ id ti = match !ti with
    | I_Zero | I_One -> ti
    | I_Var x -> if x = id then ref(I_Zero) else ti
    | I_Max(i1, i2) -> imax (force_zero_occ id i1) (force_zero_occ id i2)
  in
  match !t1, !t2 with
  | _, _ when t1 = t2 -> ()
  | I_Zero, _ -> ()
  | _, I_One -> ()
  | _, I_Zero -> force_zero t1
  | I_Max(i1, i2), _ -> force_sub i1 t1; force_sub i2 t2
  | _, I_Var id ->
    let t1 = force_zero_occ id t1 in
    t2 := !t1
  | I_Var id, I_Max(i1, i2) ->
    let i1 = force_zero_occ id i1 in
    let i2 = force_zero_occ id i2 in
    t2 := !(imax t1 (imax i1 i2))
  | I_One, I_Max _ -> error Dummy

and force_initialized env e =
  let it = itype_expr env e in
  force_sub it (ref I_Zero)

and itype_expr env expr =
  match expr.texpr_desc with
  | TE_const c -> ref I_Zero
  | TE_ident x -> Gamma.find env x
  | TE_unop (op, e) -> itype_expr env e

  | TE_binop (op, e1, e2) ->
    let ti1 = itype_expr env e1 in
    let ti2 = itype_expr env e2 in
    imax ti1 ti2

  | TE_app (f, el, r) ->
    List.iter (force_initialized env) el;
    ref I_Zero

  | TE_fby (e1, e2) ->
    force_initialized env e2;
    itype_expr env e1

  | TE_pre e ->
    force_initialized env e;
    ref I_One

  | TE_current e -> itype_expr env e

  | TE_when(e, cond, id) ->
    imax (itype_expr env e) (Gamma.find env id)

  | TE_merge(e, mat) ->
    imax
      (imax_list (List.map (fun (c, e) -> itype_expr env e) mat))
      (itype_expr env e)

  | _ -> assert false

let itype_equation env eq =
  let itpatt = List.map (Gamma.find env) eq.teq_patt.tpatt_desc in
  let itpatt = imax_list itpatt in
  let itexpr = itype_expr env eq.teq_expr in
  force_sub itexpr itpatt

let itype_node n =
  let env = Gamma.adds_zero Gamma.empty
      (List.map (fun (x, t, c) -> x) (n.tn_input@n.tn_output)) in
  let env = Gamma.adds_new env
      (List.map (fun (x, t, c) -> x) n.tn_local) in
  List.iter (itype_equation env) n.tn_equs

let itype_program nl =
  List.iter itype_node nl

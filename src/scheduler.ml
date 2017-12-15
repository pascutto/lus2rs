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
open Ast_clocked_lustre

exception Causality of location

module C = Set.Make(Ident)
let consts = ref C.empty

module S = Set.Make(Ident)
module Graph = Set.Make(
  struct
    type t = Ident.t * S.t * c_equation
    let compare (x1, s1, _) (x2, s2, _) =
      let c = Ident.compare x1 x2 in
      if c <> 0 then c else S.compare s1 s2
  end)

let add_vars_of_patt s {cpatt_desc = p} =
  List.fold_left (fun s x -> S.add x s) s p

let rec add_vars_of_exp s {cexpr_desc = e} =
  match e with
  | CE_const _ -> s
  | CE_ident x -> S.add x s
  | CE_arrow (e1, e2) -> add_vars_of_exp (add_vars_of_exp s e1) e2
  | CE_fby (e1, e2) -> add_vars_of_exp s e1
  | CE_pre e -> s
  | CE_current e -> add_vars_of_exp s e
  | CE_binop (_, e1, e2) -> add_vars_of_exp (add_vars_of_exp s e1) e2
  | CE_unop (_, e) -> add_vars_of_exp s e
  | CE_app (_,l) -> List.fold_left add_vars_of_exp s l
  | CE_prim (_,l) -> List.fold_left add_vars_of_exp s l
  | CE_tuple l -> List.fold_left add_vars_of_exp s l
  | CE_merge (id, mat) ->
    List.fold_left add_vars_of_exp (S.add id s) (List.map snd mat)
  | CE_when (e, cond, clk) -> S.add clk s

let schedule_equs nloc inputs equs =
  let g =
    List.fold_left
      (fun g eq ->
         let vp = add_vars_of_patt S.empty eq.ceq_patt in
         let ve = add_vars_of_exp S.empty eq.ceq_expr in
      	S.fold (fun x g -> Graph.add (x, ve, eq) g) vp g)
      Graph.empty equs
  in
  (* Suppression des dépendances aux entrées. *)
  let g =
    let s_inputs =
      List.fold_left (fun acc (x, _, _) -> S.add x acc) S.empty inputs
    in
    Graph.fold
      (fun (y,s,e) g -> Graph.add (y, S.diff s s_inputs, e) g)
      g
      Graph.empty
  in
  (* Tri topologique des equations *)
  let rec exists_loop topo g =
    if Graph.is_empty g then List.rev topo
    else
      let g1 , g2 = Graph.partition (fun (_,s,_) -> S.is_empty s) g in
      if Graph.is_empty g1 then raise (Causality nloc);
      let sv =
        Graph.fold (fun (x,_,_) s -> S.add x s) g1 S.empty
      in
      let g =
	Graph.fold
          (fun (y,s,e) g -> Graph.add (y,S.diff s sv,e) g)
          g2 Graph.empty
      in
      let topo =
	Graph.fold
          (fun (_,_,e) l -> if List.mem e l then l else e::l)
          g1 topo
      in
      exists_loop topo g
  in
  exists_loop [] g

let schedule_node n =
  let equs = schedule_equs n.cn_loc n.cn_input n.cn_equs in
  { n with cn_equs = equs; }

let schedule =
  List.map schedule_node

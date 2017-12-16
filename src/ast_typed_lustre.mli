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

type tclock =
  | TBase
  | TClk of Ident.t * const

and decl_var = Ident.t * base_typ * tclock

and t_expr = {
  texpr_desc: t_expr_desc;
  texpr_type: typ;
  texpr_loc: location;
}

and t_expr_desc =
  | TE_const of const
  | TE_ident of Ident.t
  | TE_binop of binop * t_expr * t_expr
  | TE_unop of unop * t_expr
  | TE_fby of t_expr * t_expr
  | TE_app of Ident.t * t_expr list * t_expr
  | TE_prim of Ident.t * t_expr list * t_expr
  | TE_pre of t_expr
  | TE_current of t_expr
  | TE_merge of t_expr * (const * t_expr) list
  | TE_when of t_expr * const * Ident.t
  | TE_tuple of t_expr list

type t_patt = {
  tpatt_desc: Ident.t list;
  tpatt_type: typ;
  tpatt_loc: location;
}

type t_equation = {
  teq_patt: t_patt;
  teq_expr: t_expr;
}

type t_node = {
  tn_name: Ident.t;
  tn_input: decl_var list;
  tn_output: decl_var list;
  tn_local: decl_var list;
  tn_equs: t_equation list;
  tn_loc: location;
}

type t_constant = {
  tc_name: Ident.t;
  tc_desc: t_expr;
  tc_type: typ;
}

type t_file = t_node list

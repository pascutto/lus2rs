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

type base_clock =
  | Base
  | Clk of base_clock * Ident.t * const

and clock = base_clock list

and c_var = Ident.t * base_typ * base_clock

and c_expr = {
  cexpr_desc: c_expr_desc;
  cexpr_type: typ;
  cexpr_clock: clock;
  cexpr_loc: location;
}

and c_expr_desc =
  | CE_const of const
  | CE_ident of Ident.t
  | CE_binop of binop * c_expr * c_expr
  | CE_unop of unop * c_expr
  | CE_arrow of c_expr * c_expr
  | CE_fby of c_expr * c_expr
  | CE_app of Ident.t * c_expr list
  | CE_prim of Ident.t * c_expr list (* ?? *)
  | CE_pre of c_expr
  | CE_current of c_expr
  | CE_merge of Ident.t * (const * c_expr) list
  | CE_when of c_expr * const * Ident.t
  | CE_tuple of c_expr list

type c_patt = {
  cpatt_desc: Ident.t list;
  cpatt_type: typ;
  cpatt_clock: clock;
  cpatt_loc: location;
}

type c_equation = {
  ceq_patt: c_patt;
  ceq_expr: c_expr;
}

type c_node = {
  cn_name: Ident.t;
  cn_input: c_var list;
  cn_output: c_var list;
  cn_local: c_var list;
  cn_equs: c_equation list;
  cn_loc: location;
}

type c_file = c_node list

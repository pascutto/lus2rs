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

type pclock =
  | PBase
  | PClk of ident * ls_expr

and ls_expr = {
  pexpr_desc: ls_expr_desc;
  pexpr_loc: location;
}

and ls_expr_desc =
  | LSE_const of const
  | LSE_ident of ident
  | LSE_binop of binop * ls_expr * ls_expr
  | LSE_unop of unop * ls_expr
  | LSE_app of ident * ls_expr list
  | LSE_arrow of ls_expr * ls_expr
  | LSE_fby of ls_expr * ls_expr
  | LSE_pre of ls_expr
  | LSE_current of ls_expr
  | LSE_merge of ident * (ls_expr * ls_expr) list
  | LSE_when of ls_expr * ls_expr * ident
  | LSE_tuple of ls_expr list
  | LSE_if of ls_expr * ls_expr * ls_expr

and ls_patt = {
  ppatt_desc: ls_patt_desc;
  ppatt_loc: location;
}

and ls_patt_desc = ident list

and ls_equation = {
  lseq_patt: ls_patt;
  lseq_expr: ls_expr;
}

and ls_element =
  | LS_Node of ls_node
  | LS_Constant of ls_constant

and ls_node = {
  lsn_name: ident;
  lsn_inputs: (ident * base_typ * pclock) list;
  lsn_outputs: (ident * base_typ * pclock) list;
  lsn_locals: (ident * base_typ * pclock) list;
  lsn_eqs: ls_equation list;
  lsn_loc: location;
}

and ls_constant = {
  lsc_name: ident;
  lsc_desc: ls_expr;
}

and ls_file = ls_element list

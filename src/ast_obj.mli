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

type o_patt = Ident.t * base_typ

type o_expr =
  | OE_Ident of Ident.t
  | OE_Const of const
  | OE_State of Ident.t
  | OE_Binop of binop * o_expr * o_expr
  | OE_Unop of unop * o_expr

and o_statement =
  | OS_Var_assign of Ident.t * o_expr
  | OS_State_assign of Ident.t * o_expr
  | OS_Sequence of o_statement * o_statement
  | OS_Skip
  | OS_Reset of int
  | OS_Step of Ident.t list * int * o_expr list
  | OS_Case of o_expr * (const * o_statement) list

type o_inst = int * Ident.t

and o_class = {
  oc_name: Ident.t;
  oc_mem: o_patt list;
  oc_instances: o_inst list;
  oc_reset: o_statement;
  oc_step: o_patt list * o_patt list * o_patt list * o_statement
}

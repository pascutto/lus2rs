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

type location = Lexing.position * Lexing.position

type ident = string

type base_typ =
  | Tbool
  | Tint
  | Treal

type typ = base_typ list

type const =
  | Cbool of bool
  | Cint of int
  | Creal of float

type binop =
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge | Op_add | Op_sub | Op_mul
  | Op_div | Op_mod | Op_and | Op_or | Op_impl
  | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f

type unop =
  | Op_not | Op_uminus
  | Op_uminus_f

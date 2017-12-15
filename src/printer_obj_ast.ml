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

open Format
open Ast_types_lustre
open Ast_obj

let rec print_list f sep fmt = function
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let print_const fmt = function
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let print_binop fmt = function
  | Op_eq -> fprintf fmt "="
  | Op_neq -> fprintf fmt "<>"
  | Op_lt -> fprintf fmt "<"
  | Op_le -> fprintf fmt "<="
  | Op_gt -> fprintf fmt ">"
  | Op_ge -> fprintf fmt ">="
  | Op_add | Op_add_f -> fprintf fmt "+"
  | Op_sub | Op_sub_f -> fprintf fmt "-"
  | Op_mul | Op_mul_f -> fprintf fmt "*"
  | Op_div | Op_div_f -> fprintf fmt "/"
  | Op_mod -> fprintf fmt "mod"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "=>"

let print_unop fmt = function
  | Op_not -> fprintf fmt "not"
  | Op_uminus | Op_uminus_f -> fprintf fmt "-"

let rec print_expr fmt = function
  | OE_Ident x -> fprintf fmt "%a" Ident.print x
  | OE_Const c -> print_const fmt c
  | OE_State x -> fprintf fmt "state(%a)" Ident.print x
  | OE_Binop (op, e1, e2) ->
    fprintf fmt "@[(@[%a@]) @[%a@] (@[%a@])@]"
      print_expr e1 print_binop op print_expr e2
  | OE_Unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_expr e

and print_statement fmt = function
  | OS_Var_assign (id, e) | OS_State_assign (id, e) ->
    fprintf fmt "@[(@[%a@]) := (@[%a@])@]" Ident.print id print_expr e
  | OS_Sequence (s1, s2) ->
    fprintf fmt "@[@[%a;@]@\n @[%a@]@]" print_statement s1 print_statement s2
  | OS_Skip -> fprintf fmt "skip"
  | OS_Reset id -> fprintf fmt "@[%i.reset()@]" id
  | OS_Step (idl, id, el) ->
    fprintf fmt "@[@[(%a)@] = @[%i.step(@[%a@])@]@]"
      print_tuple_list idl
      id
      print_arg_list el
  | OS_Case (id, csl) ->
    fprintf fmt "@[case (@[%a@]) @{%a@}@]"
      Ident.print id
      print_matching csl

and print_arg_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_expr x
  | h :: t -> fprintf fmt "%a,@ %a" print_expr h print_arg_list t

and print_matching fmt = function
  | [] -> ()
  | (c, e)::t -> fprintf fmt "@[(@[%a@] : @[%a@])@] @ %a"
                   print_const c print_statement e print_matching t

and print_tuple_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" Ident.print x
  | h :: t -> fprintf fmt "%a,@ %a" Ident.print h print_tuple_list t

let print_base_type fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

let print_var_dec fmt (name, ty) =
  fprintf fmt "%a : %a" Ident.print name print_base_type ty

let print_instance fmt (id, cl) =
  fprintf fmt "@[%i : %a@]" id Ident.print cl

let print_var_dec_list = print_list print_var_dec ";"

let print_instance_list = print_list print_instance ","

let print_class fmt c =
  let inp, out, loc, st = c.oc_step in
  fprintf fmt
    "@[machine %a =@\nmemory [@[%a@]]@\ninstances [@[%a@]]@\nreset() = @[%a@]@\n@[step (@[%a@])@] @[returns (@[%a@])@] = @[var @[%a;@]@] in @[<v 2>@[%a@]@]@]"
    Ident.print c.oc_name
    print_var_dec_list c.oc_mem
    print_instance_list c.oc_instances
    print_statement c.oc_reset
    print_var_dec_list inp
    print_var_dec_list out
    print_var_dec_list loc
    print_statement st

let print_program cl =
  List.iter (fun c -> Format.printf "%a@\n@." print_class c) cl

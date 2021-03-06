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
open Ast_clocked_lustre

let verbose = ref false

let print_const fmt = function
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let rec print_base_type fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

and print_base_clock fmt = function
  | Base -> fprintf fmt "base"
  | Clk (ck, id, cond) ->
    fprintf fmt "(%a on %a(@[%a@]))"
      print_base_clock ck print_const cond Ident.print id

let rec print_list f sep fmt = function
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let print_clock = print_list print_base_clock ","

let rec print_list_eol f sep fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

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

let rec print_exp fmt e =
  if !verbose
  then
    fprintf fmt "%a :: %a" print_exp_desc e.cexpr_desc print_clock e.cexpr_clock
  else fprintf fmt "%a" print_exp_desc e.cexpr_desc

and print_exp_desc fmt = function
  | CE_const c -> print_const fmt c
  | CE_ident x -> fprintf fmt "%a" Ident.print x
  | CE_binop (op, e1, e2) ->
    fprintf fmt "@[(@[%a@]) @[%a@] (@[%a@])@]"
      print_exp e1 print_binop op print_exp e2
  | CE_unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_exp e
  | CE_app (name, e_list, reset) | CE_prim (name, e_list, reset) ->
    fprintf fmt "@[%a(@[%a@]) every %a@]"
      Ident.print name print_arg_list e_list print_exp reset
  | CE_fby (l, r) ->
    fprintf fmt "@[(@[%a@]) fby (@[%a@])@]" print_exp l print_exp r
  | CE_pre e ->
    fprintf fmt "pre (@[%a@])" print_exp e
  | CE_current e ->
    fprintf fmt "current (@[%a@])" print_exp e
  | CE_when (e, cond, clk) ->
    fprintf fmt "@[(@[%a@]) when @[%a@](@[%a@])@]"
      print_exp e print_const cond Ident.print clk
  | CE_merge(clk,le) ->
    fprintf fmt "merge @[%a@] @[%a@]" print_exp clk print_matching le
  | CE_tuple e_list ->
    fprintf fmt "(@[%a@])" print_tuple_arg_list e_list

and print_arg_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_matching fmt = function
  | [] -> ()
  | (c, e)::t -> fprintf fmt "@[(@[%a@] -> @[%a@])@] @ %a"
                   print_const c print_exp e print_matching t

and print_tuple_arg_list fmt = function
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt = function
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list Ident.print ",") eq.ceq_patt.cpatt_desc
    print_exp eq.ceq_expr

let print_var_dec fmt (name, ty, clk) = match clk with
  | Base -> fprintf fmt "%a : %a" Ident.print name print_base_type ty
  | Clk(ck, id, cond) -> fprintf fmt "(%a : %a :: %a on %a(%a))"
                           Ident.print name
                           print_base_type ty
                           print_base_clock ck
                           print_const cond
                           Ident.print id

let rec print_var_dec_list = print_list print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print nd.cn_name
    print_var_dec_list nd.cn_input
    print_var_dec_list nd.cn_output
    print_var_dec_list nd.cn_local
    (print_list_eol print_eq ";") nd.cn_equs

let print_program ndl =
  verbose := false;
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl

let print_program_v ndl =
  verbose := true;
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl

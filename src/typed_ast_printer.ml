(* code d'Adrien Guatto *)

open Format
open Lustre_ast_types
open Lustre_typed_ast

let rec print_list f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let print_const fmt c = match c with
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Creal f -> fprintf fmt "%f" f

let print_binop fmt op = match op with
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

let print_unop fmt op = match op with
  | Op_not -> fprintf fmt "not"
  | Op_uminus | Op_uminus_f -> fprintf fmt "-"

let rec print_exp fmt e = match e.texpr_desc with
  | TE_const c -> print_const fmt c
  | TE_ident x -> fprintf fmt "%a" Ident.print x
  | TE_binop (op, e1, e2) ->
    fprintf fmt "@[(@[%a@]) @[%a@] (@[%a@])@]"
      print_exp e1 print_binop op print_exp e2
  | TE_unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_exp e
  | TE_if (e1, e2, e3) ->
    fprintf fmt "@[if (@[%a@]) then (@[%a@]) else (@[%a@])@]"
      print_exp e1 print_exp e2 print_exp e3
  | TE_app (name, e_list) | TE_prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" Ident.print name print_arg_list e_list
  | TE_arrow (l, r) ->
    fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_exp l print_exp r
  | TE_fby (l, r) ->
      fprintf fmt "@[(@[%a@]) fby (@[%a@])@]" print_exp l print_exp r
  | TE_pre e ->
      fprintf fmt "pre (@[%a@])" print_exp e
  | TE_current e ->
      fprintf fmt "current (@[%a@])" print_exp e
  | TE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list Ident.print ",") eq.teq_patt.tpatt_desc
    print_exp eq.teq_expr

let print_base_type fmt bty = match bty with
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

(* let print_type = print_list print_cbase_type "*" *)

let print_var_dec fmt (name, ty) =
  fprintf fmt "%a : %a" Ident.print name print_base_type ty

let rec print_var_dec_list = print_list print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print nd.tn_name
    print_var_dec_list nd.tn_input
    print_var_dec_list nd.tn_output
    print_var_dec_list nd.tn_local
    (print_list_eol print_eq ";") nd.tn_equs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl

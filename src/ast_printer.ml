open Format
open Lustre_ast_types
open Lustre_ast

let rec print_list f sep fmt = function
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_ls_patt_desc fmt = function
  | LSP_ident id -> fprintf fmt "%s" id
  | LSP_tuple l -> print_list (fun x -> fprintf fmt "%s") "," fmt l

let rec print_list_eol f sep fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

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

let rec print_exp fmt e = match e.pexpr_desc with
  | LSE_const c -> print_const fmt c
  | LSE_ident x -> fprintf fmt "%s" x
  | LSE_binop (op, e1, e2) ->
    fprintf fmt "@[(@[%a@]) @[%a@] (@[%a@])@]"
      print_exp e1 print_binop op print_exp e2
  | LSE_unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_exp e
  | LSE_if (e1, e2, e3) ->
    fprintf fmt "@[if (@[%a@]) then (@[%a@]) else (@[%a@])@]"
      print_exp e1 print_exp e2 print_exp e3
  | LSE_app (name, e_list) ->
      fprintf fmt "%s(@[%a@])" name print_arg_list e_list
  | LSE_arrow (l, r) ->
    fprintf fmt "@[(@[%a@]) -> (@[%a@])@]" print_exp l print_exp r
  | LSE_fby (l, r) ->
      fprintf fmt "@[(@[%a@]) fby (@[%a@])@]" print_exp l print_exp r
  | LSE_pre e ->
      fprintf fmt "pre (@[%a@])" print_exp e
  | LSE_current e ->
    fprintf fmt "current (@[%a@])" print_exp e
  | LSE_when (e, cond, clk) -> fprintf fmt "@[(@[%a@]) when @[%a@](@[%a@])@]" print_exp e print_exp cond print_exp clk
  | LSE_merge(clk,le) ->
    fprintf fmt "merge @[%s@] @[%a@]" clk print_matching le
  | LSE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list

and print_arg_list fmt = function
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_matching fmt = function
  | [] -> ()
  | (c, e)::t -> fprintf fmt "@[(@[%a@] -> @[%a@])@] @ %a"
                   print_exp c print_exp e print_matching t

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
    print_ls_patt_desc eq.lseq_patt.ppatt_desc
    print_exp eq.lseq_expr

let print_base_type fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

let print_var_dec fmt (name, ty) =
  fprintf fmt "%s : %a" name print_base_type ty

let print_var_dec_list = print_list print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %s(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    nd.lsn_name
    print_var_dec_list nd.lsn_inputs
    print_var_dec_list nd.lsn_outputs
    print_var_dec_list nd.lsn_locals
    (print_list_eol print_eq ";") nd.lsn_eqs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl
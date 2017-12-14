open Ast_types_lustre

type o_patt = {
  op_name: Ident.t;
  op_type: typ;
}

type o_expr =
  | OE_ident of Ident.t
  | OE_const of const
  | OE_state of Ident.t
  | OE_binop of binop * o_expr * o_expr
  | OE_unop of unop * o_expr

and o_statement =
  | OS_var_assign of Ident.t * o_expr
  | OS_state_assign of Ident.t * o_expr
  | OS_sequence of o_statement * o_statement
  | OS_skip
  | OS_reset of Ident.t
  | OS_step of Ident.t list * Ident.t * o_expr list
  | OS_case of Ident.t * (Ident.t * o_statement) list

type o_inst = {
  oi_name: Ident.t;
  oi_class: Ident.t
}

and o_class = {
  oc_name: Ident.t;
  oc_mem: o_patt list;
  oc_instances: o_inst list;
  oc_reset: o_statement;
}

type o_const = {
  oc_name: Ident.t;
  oc_value: const
}

type o_element =
  | Oclass of o_class
  | Oconst of o_const

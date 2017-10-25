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

%{
  open Lustre_ast
  open Lustre_ast_types
  open Parsing

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () }
  let mk_patt p = { ppatt_desc = p; ppatt_loc = loc () }

%}

%token AND ARROW BOOL COLON COMMA LE LT GE GT FBY
%token DIV ELSE EQUALS NEQ REAL IF IMPL INT LET LPAR MINUS MOD NODE NOT OR %token PLUS PRE CURRENT RETURNS RPAR SEMICOLON TIMES TEL THEN VAR
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> REAL_CONST
%token <string> IDENT
%token EOF

%nonassoc ELSE
%right ARROW
%right FBY
%left IMPL
%left OR
%left AND
%left LE LT GE GT EQUALS NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc CURRENT NOT PRE

%start file
%type <Lustre_ast.ls_file> file

%%

file:
  node* EOF { $1 }
;

node:
  NODE name = IDENT LPAR ins = separated_list(SEMICOLON, param) RPAR
  RETURNS LPAR outs = separated_list(SEMICOLON, param) RPAR SEMICOLON
  locals = local_params
  LET eqs = equation+ TEL SEMICOLON?
                                  { {
                                    lsn_name = name;
                                  	lsn_inputs = List.flatten ins;
                                  	lsn_outputs = List.flatten outs;
                                  	lsn_locals = locals;
                                  	lsn_eqs = eqs;
                                  	lsn_loc = loc();
                                  } }
;

local_params:
  | /* empty */                   { [] }
  | VAR local_param_list          { $2 }
;

local_param_list:
  | param SEMICOLON               { $1 }
  | param SEMICOLON local_param_list
                                  { $1 @ $3 }
;

param:
  separated_nonempty_list(COMMA, IDENT) COLON typ
                                  { List.map (fun id -> (id, $3)) $1 }
;

equation:
  pattern EQUALS expr SEMICOLON   { { lseq_patt = $1; lseq_expr = $3; } }
;

pattern:
  | IDENT                         { mk_patt (LSP_ident $1) }
  | LPAR separated_nonempty_list(COMMA, IDENT) RPAR
                                  { mk_patt (LSP_tuple $2) }
;

expr:
  | LPAR expr RPAR                { $2 }
  | const                         { $1 }
  | IDENT                         { mk_expr (LSE_ident $1)}
  | IDENT LPAR separated_list(COMMA, expr) RPAR
                                  { mk_expr (LSE_app ($1, $3))}
  | IF expr THEN expr ELSE expr   { mk_expr (LSE_if ($2, $4, $6)) }
  | expr FBY expr                 { mk_expr (LSE_fby ($1, $3)) }
  | expr PLUS expr                { mk_expr (LSE_binop (Op_add, $1, $3)) }
  | expr MINUS expr               { mk_expr (LSE_binop (Op_sub, $1, $3)) }
  | expr TIMES expr               { mk_expr (LSE_binop (Op_mul, $1, $3)) }
  | expr DIV expr                 { mk_expr (LSE_binop (Op_div, $1, $3)) }
  | expr MOD expr                 { mk_expr (LSE_binop (Op_mod, $1, $3)) }
  | expr LE expr                  { mk_expr (LSE_binop (Op_le, $1, $3)) }
  | expr LT expr                  { mk_expr (LSE_binop (Op_lt, $1, $3)) }
  | expr GE expr                  { mk_expr (LSE_binop (Op_ge, $1, $3)) }
  | expr GT expr                  { mk_expr (LSE_binop (Op_gt, $1, $3)) }
  | expr NEQ expr                 { mk_expr (LSE_binop (Op_neq, $1, $3)) }
  | expr EQUALS expr              { mk_expr (LSE_binop (Op_eq, $1, $3)) }
  | expr AND expr                 { mk_expr (LSE_binop (Op_and, $1, $3)) }
  | expr OR expr                  { mk_expr (LSE_binop (Op_or, $1, $3)) }
  | expr IMPL expr                { mk_expr (LSE_binop (Op_impl, $1, $3)) }
  | expr ARROW expr               { mk_expr (LSE_arrow ($1, $3)) }
  | MINUS expr                    { mk_expr (LSE_unop (Op_uminus, $2)) }
  | NOT expr                      { mk_expr (LSE_unop (Op_not, $2)) }
  | PRE expr                      { mk_expr (LSE_pre $2) }
  | CURRENT expr                  { mk_expr (LSE_current $2) }
  | LPAR expr COMMA separated_nonempty_list(COMMA, expr) RPAR
                                  { mk_expr (LSE_tuple ($2::$4)) }
;

const:
  | BOOL_CONST                    { mk_expr (LSE_const (Cbool $1)) }
  | INT_CONST                     { mk_expr (LSE_const (Cint $1)) }
  | REAL_CONST                    { mk_expr (LSE_const (Creal $1)) }
;

typ:
  | BOOL                          { Tbool }
  | INT                           { Tint }
  | REAL                          { Treal }
;
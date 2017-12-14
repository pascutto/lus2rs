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
  open Ast_lustre
  open Ast_types_lustre
  open Parsing

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () }
  let mk_patt p = { ppatt_desc = p; ppatt_loc = loc () }

%}

%token AND ARROW BOOL COLON COMMA LE LT GE GT FBY WHEN MERGE
%token DIV ELSE EQUALS NEQ REAL IF IMPL INT LET LPAR MINUS MOD NODE NOT OR %token PLUS PRE CURRENT RETURNS RPAR SEMICOLON TIMES TEL THEN VAR CONST
%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <float> REAL_CONST
%token <string> IDENT
%token EOF

%nonassoc ELSE
%right ARROW FBY
%left WHEN
%left IMPL
%left OR
%left AND
%left LE LT GE GT EQUALS NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc CURRENT NOT PRE

%start file
%type <Ast_lustre.ls_file> file

%%

file:
  element* EOF { $1 }
;

element:
  | node                          { $1 }
  | const_decl                    { $1 }

const_decl:
  CONST IDENT EQUALS expr SEMICOLON
                                  { LS_Constant {
                                    lsc_name = $2;
                                    lsc_desc = $4;
                                  } }

node:
  NODE name = IDENT LPAR ins = separated_list(SEMICOLON, param) RPAR
  RETURNS LPAR outs = separated_list(SEMICOLON, param) RPAR SEMICOLON
  locals = local_params
  LET eqs = equation+ TEL SEMICOLON?
                                  { LS_Node {
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
  | VAR separated_nonempty_list(VAR, local_param_list)
                                  { List.flatten $2 }
;

local_param_list:
  | param SEMICOLON               { $1 }
  | param SEMICOLON local_param_list
                                  { $1 @ $3 }
;

param:
  | separated_nonempty_list(COMMA, IDENT) COLON typ
                                  { List.map (fun id -> (id, $3, PBase)) $1 }
  | separated_nonempty_list(COMMA, IDENT) COLON typ WHEN const LPAR IDENT RPAR
                                  {
                                    List.map
                                      (fun id -> (id, $3, PClk($7, $5)))
                                      $1
                                  }
  | separated_nonempty_list(COMMA, IDENT) COLON typ WHEN IDENT
                                  {
                                    List.map
                                      (fun id -> (id, $3, PClk($5,
                                        mk_expr (LSE_const(Cbool true)))))
                                      $1
                                  }
;

equation:
  pattern EQUALS expr SEMICOLON   { { lseq_patt = $1; lseq_expr = $3; } }
;

pattern:
  | separated_nonempty_list(COMMA, IDENT)
                                  { mk_patt ($1) }
  | LPAR pattern RPAR
                                  { $2 }

;

expr:
  | LPAR expr RPAR                { $2 }
  | const                         { $1 }
  | IDENT                         { mk_expr (LSE_ident $1) }
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
  | expr WHEN const LPAR IDENT RPAR
                                  { mk_expr (LSE_when ($1, $3, $5)) }
  | expr WHEN IDENT
                                  {
                                    mk_expr (LSE_when ($1,
                                      mk_expr (LSE_const (Cbool true)),
                                    $3))
                                  }
  | MERGE IDENT matching+         { mk_expr (LSE_merge ($2, $3)) }
  | MINUS expr                    { mk_expr (LSE_unop (Op_uminus, $2)) }
  | NOT expr                      { mk_expr (LSE_unop (Op_not, $2)) }
  | PRE expr                      { mk_expr (LSE_pre $2) }
  | CURRENT expr                  { mk_expr (LSE_current $2) }
  | LPAR expr COMMA separated_nonempty_list(COMMA, expr) RPAR
                                  { mk_expr (LSE_tuple ($2::$4)) }
;

matching: LPAR const ARROW expr RPAR
                                  { $2, $4 }

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

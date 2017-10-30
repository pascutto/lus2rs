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

{
  open Lexing
  open Lustre_parser
  open Lustre_ast

  exception Lexing_error of string

  let keywords = Hashtbl.create 21
  let () = List.iter (fun (key, value) -> Hashtbl.add keywords key value)
    [
    	"and", AND;
    	"bool", BOOL;
      "div", DIV;
    	"else", ELSE;
    	"false", BOOL_CONST(false);
    	"if", IF;
    	"int", INT;
    	"let", LET;
    	"mod", MOD;
    	"node", NODE;
    	"not", NOT;
    	"or", OR;
      "pre", PRE;
      "fby", FBY;
      "current", CURRENT;
    	"real", REAL;
    	"returns", RETURNS;
    	"tel", TEL;
    	"then", THEN;
    	"true", BOOL_CONST(true);
    	"var", VAR;
      "when", WHEN;
      "merge", MERGE
    ]
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let regfloat =
  digit+ '.' digit* exponent?
  | digit* '.' digit+ exponent?
	| digit+ exponent
let regint = ('0' | ['1'-'9'] digit*)
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | '\n'                          { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+              { token lexbuf }
  | "--" [^'\n']* '\n'            { new_line lexbuf; token lexbuf }
  | "/*"                          { comment lexbuf }
  | ident as id
                                  {
                                    try Hashtbl.find keywords id with Not_found
                                    -> IDENT id
                                  }
  | regint as num
                                  {
                                    let i = try int_of_string num
                                      with _ ->
                                      raise (Lexing_error "int overflow")
                                    in
                                    INT_CONST i
                                  }
  | regfloat as num
                                  {
                                    let i = try float_of_string num
                                      with _ ->
                                      raise (Lexing_error "float overflow")
                                    in
                                    REAL_CONST i
                                  }
  | "-"                           { MINUS }
  | "+"                           { PLUS }
  | "*"                           { TIMES }
  | "/"                           { DIV }
  | ">"                           { GT }
  | ">="                          { GE }
  | "<"                           { LT }
  | "<="                          { LE }
  | "<>"                          { NEQ }
  | "=>"                          { IMPL }
  | "->"                          { ARROW }
  | "("                           { LPAR }
  | ")"                           { RPAR }
  | ":"                           { COLON }
  | ";"                           { SEMICOLON }
  | "="                           { EQUALS }
  | ","                           { COMMA }
  | _                             { raise (Lexing_error (lexeme lexbuf)) }
  | eof                           { EOF }

and comment = parse
  | "*/"                          { token lexbuf }
  | '\n'                          { new_line lexbuf; comment lexbuf }
  | _                             { comment lexbuf }
  | eof                           { raise (Lexing_error "unterminated comment") }

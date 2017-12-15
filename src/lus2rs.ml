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
open Lexing
open Lexer_lustre
open Parser_lustre
open Ast_lustre
open Ast_types_lustre

let usage = "usage: "^Sys.argv.(0)^" [options] file.lus main"

let parse_only = ref false
let type_only = ref false
let clock_only = ref false
let norm_only = ref false
let schedule_only = ref false
let obj_only = ref false
let verbose = ref false

let spec =
  ["--parse-only", Arg.Set parse_only, "  stops after parsing";
   "-p", Arg.Set parse_only, "  stops after parsing";
   "--type-only", Arg.Set type_only, "  stops after typing";
   "-t", Arg.Set type_only, "  stops after typing";
   "--clock-only", Arg.Set clock_only, "  stops after clocking";
   "-c", Arg.Set clock_only, "  stops after clocking";
   "--schedule-only", Arg.Set schedule_only, "  stops after scheduling";
   "-s", Arg.Set schedule_only, "  stops after scheduling";
   "--norm-only", Arg.Set norm_only, "  stops after normalization";
   "-n", Arg.Set norm_only, "  stops after normalization";
   "--obj-only", Arg.Set obj_only, "  stops after obj translation";
   "-o", Arg.Set obj_only, "  stops after obj translation";
   "--verbose", Arg.Set verbose, "print intermediate transformations";
   "-v", Arg.Set verbose, "print intermediate transformations";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".lus") then
      raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !file with Some f -> f | None -> Arg.usage spec usage; exit 1)

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    (* Parsing *)
    let f = Parser_lustre.file Lexer_lustre.token lb in
    close_in c;
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*            Parsed ast              */@.";
      Format.printf "/**************************************/@.";
      Printer_ast.print_program f
    end;

    (* Sugar elimination *)
    let fsu = Sugar.transform f in
    close_in c;
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*       Parsed ast (sugarless)       */@.";
      Format.printf "/**************************************/@.";
      Printer_ast.print_program fsu
    end;

    if !parse_only then exit 0;

    (* Typing *)
    let ft = Typer.type_program fsu in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*             Typed ast              */@.";
      Format.printf "/**************************************/@.";
      Printer_typed_ast.print_program_v ft
    end;
    if !type_only then exit 0;

    (* Clocking *)
    let fc = Clocker.clock_program ft in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*            Clocked ast             */@.";
      Format.printf "/**************************************/@.";
      Printer_clocked_ast.print_program_v fc
    end;
    if !clock_only then exit 0;

    (* Normalizing *)
    let fn = Normalizer.normalize_file fc in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*           Normalized ast           */@.";
      Format.printf "/**************************************/@.";
      Printer_clocked_ast.print_program fn
    end;
    if !norm_only then exit 0;

    (* Scheduling *)
    let fs = Scheduler.schedule fn in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*           Scheduled ast            */@.";
      Format.printf "/**************************************/@.";
      Printer_clocked_ast.print_program fs
    end;
    if !schedule_only then exit 0;

    (* Transforming *)
    let fo = Translater.translate fs in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*             Object ast             */@.";
      Format.printf "/**************************************/@.";
      Printer_obj_ast.print_program fo
    end;
    if !obj_only then exit 0;

  with
    | Lexing_error s ->
    	report_loc (lexeme_start_p lb, lexeme_end_p lb);
    	eprintf "Lexical error: %s\n@." s;
    	exit 1
    | Parser_lustre.Error ->
    	report_loc (lexeme_start_p lb, lexeme_end_p lb);
    	eprintf "Syntax error\n@.";
    	exit 1
    | Typer.Error(l,e) ->
    	report_loc l;
    	eprintf "Type error: %a\n@." Typer.report e;
     exit 1
    | Clocker.Error(l,e) ->
      report_loc l;
      eprintf "Clock error: %a\n@." Clocker.report e;
    exit 1
    | Scheduler.Causality(l) ->
      report_loc l;
      eprintf "Node not schedulable\n@.";
      exit 1
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2

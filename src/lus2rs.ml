
(* Programme principal *)

open Format
open Lexing
open Lustre_lexer
open Lustre_parser
open Lustre_ast
open Lustre_ast_types

let usage = "usage: "^Sys.argv.(0)^" [options] file.lus main"

let parse_only = ref false
let type_only = ref false
let schedule_only = ref false
let norm_only = ref false
let verbose = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-type-only", Arg.Set type_only, "  stops after typing";
   "-schedule-only", Arg.Set type_only, "  stops after scheduling";
   "-norm-only", Arg.Set norm_only, "  stops after normalization";
   "-verbose", Arg.Set verbose, "print intermediate transformations";
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
    let f = Lustre_parser.file Lustre_lexer.token lb in
    close_in c;
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*            Parsed ast              */@.";
      Format.printf "/**************************************/@.";
      Ast_printer.print_program f
    end;
    if !parse_only then exit 0;
    let ft = Typer.type_file f in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*             Typed ast              */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_program_v ft
    end;
    if !type_only then exit 0;

    let fn = Normalizer.normalize_file ft in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*           Normalized ast           */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_program fn
    end;
    if !norm_only then exit 0;

    let fs = Scheduler.schedule fn in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/*           Scheduled ast            */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_program fs
    end;
    if !schedule_only then exit 0;

  with
    | Lexing_error s ->
    	report_loc (lexeme_start_p lb, lexeme_end_p lb);
    	eprintf "lexical error: %s\n@." s;
    	exit 1
    | Lustre_parser.Error ->
    	report_loc (lexeme_start_p lb, lexeme_end_p lb);
    	eprintf "syntax error\n@.";
    	exit 1
    | Typer.Error(l,e) ->
    	report_loc l;
    	eprintf "%a\n@." Typer.report e;
     exit 1
    | Scheduler.Causality(l) ->
      report_loc l;
      eprintf "node not schedulable\n@.";
      exit 1
    | e ->
        eprintf "Anomaly: %s\n@." (Printexc.to_string e);
        exit 2

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

open Ast_types_lustre
open Ast_lustre
open Ast_typed_lustre
open Format

module S = Set.Make(Ident)
module M = Map.Make(String)

type error =
  | ExpectedType of typ * typ
  | ExpectedPattern of typ
  | ExpectedBase of typ
  | ExpectedNum of typ
  | UnboundVar of string
  | UnboundNode of string
  | TooFewArguments
  | TooManyArguments
  | Clash of string
  | ConstantExpected
  | Other of string
  | FlatTuple
  | UndefinedOutputs of string list
  | InputVar of string

exception Error of location * error

let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let print_base_typpe fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

let print_type fmt = function
  | ([]) -> fprintf fmt "empty tuple"
  | [t] -> print_base_typpe fmt t
  | (t::tl) ->
      fprintf fmt "(";
      print_base_typpe fmt t;
      List.iter (fun t -> fprintf fmt " * %a" print_base_typpe t) tl;
      fprintf fmt ")"

let report fmt = function
  | UnboundVar id -> fprintf fmt "unbound variable %s" id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | ExpectedType (t1,t2) ->
      fprintf fmt
      "this expression has type %a but is expected to have type %a"
      print_type t1 print_type t2
  | ExpectedPattern typ ->
      fprintf fmt "this pattern is expected to have type %a"
        print_type typ
  | ExpectedBase typ ->
      fprintf fmt
     "this expression has type %a but is expected to have a type simple type"
      print_type typ
  | ExpectedNum typ ->
      fprintf fmt
      "this expression has type %a but is expected to have type int or real"
      print_type typ
  | Clash id -> fprintf fmt "The variable %s is defined several times" id
  | TooFewArguments -> fprintf fmt "too few arguments"
  | TooManyArguments -> fprintf fmt "too many arguments"
  | ConstantExpected -> fprintf fmt "this expression sould be a constant"
  | Other s -> fprintf fmt "%s" s
  | FlatTuple -> fprintf fmt "nested tuples are forbidden"
  | UndefinedOutputs l ->
      fprintf fmt "those output variables are undefined:%a"
	(fun fmt -> List.iter (fun x -> fprintf fmt "%s " x)) l
  | InputVar s -> fprintf fmt "%s is an input variable" s

let int_of_real = Ident.make "int_of_real" Ident.Prim
let real_of_int = Ident.make "real_of_int" Ident.Prim

module Delta = struct

  let prims = [
    "int_of_real", (int_of_real, ([Treal] , [Tint])) ;
    "real_of_int", (real_of_int, ([Tint] , [Treal])) ; ]

  let nodes = Hashtbl.create 97

  let is_primitive f = List.mem_assoc f prims

  let find n =
    try Hashtbl.find nodes n, false with
	Not_found -> List.assoc n prims , true

  let add x t =
    let x' = Ident.make x Ident.Node in
    Hashtbl.replace nodes x (x', t);
    x'

  let save () = Hashtbl.fold (fun key (_, typ) env -> (key,typ)::env) nodes []
end

type io = Vinput | Vpatt
module Gamma = struct

  type t = (Ident.t * base_typ * io) M.t

  let empty = M.empty

  let add loc env x t io =
    if M.mem x env then error loc (Clash x);
    let x' = Ident.make x Ident.Stream in
    M.add x (x', t, io) env

  let adds loc io =
    List.fold_left (fun env (x, t, c) -> add loc env x t io)

  let find loc env x = try
    M.find x env
    with Not_found -> error loc (UnboundVar x)

  let patts_vars env =
    M.fold (fun _ (x,_,io) s -> if io=Vpatt then S.add x s else s) env S.empty

end

let base_typ_of_ty loc t =
  match t with
  | [t'] -> t'
  | _ -> error loc (ExpectedBase t)

let compatible_base actual_ty expected_ty =
  actual_ty = expected_ty

let compatible actual_ty expected_ty =
  try
    List.fold_left2
      (fun well_t ac_t ex_t ->
	let well_t' = compatible_base ac_t ex_t in
	(well_t && well_t'))
      true actual_ty expected_ty
  with Invalid_argument _ -> false

let real_expr_of_expr te =
  match te.texpr_type with
  | [Treal] -> te
  | [Tint] ->
      { texpr_desc = TE_prim (real_of_int,[te]);
	texpr_type = [Treal];
	texpr_loc = (Lexing.dummy_pos,Lexing.dummy_pos);
      }
  | _ -> assert false

let real_op_of_int_op op =
  match op with
  | Op_add -> Op_add_f
  | Op_sub -> Op_sub_f
  | Op_mul -> Op_mul_f
  | Op_div -> Op_div_f
  | _ -> op

let not_a_nested_tuple e loc =
  match e with
    | LSE_tuple el ->
	List.iter
	  (fun e ->
	     match e.pexpr_desc with
		 LSE_tuple _ -> error loc FlatTuple;
	       | _ -> ()) el
    | _ -> assert false

let rec is_constant env e =
  match e.texpr_desc with
  | TE_const _ -> true
  | TE_tuple el -> List.for_all (is_constant env) el
  | _ -> false

let rec const_of_expr e =
  match e.texpr_desc with
  | TE_const c -> [c]
  | TE_tuple el ->
      List.fold_right (fun e acc -> const_of_expr e @ acc) el []
  | _ -> assert false

let type_const = function
  | Cbool _ -> [Tbool]
  | Cint _ -> [Tint]
  | Creal _ -> [Treal]

let rec all_same loc = function
  | [] -> assert false
  | [x] -> x
  | h::t -> if all_same loc t = h then h else error loc (ExpectedPattern h)

let rec type_expr env e =
  let desc,t = type_expr_desc env e.pexpr_loc e.pexpr_desc in
  { texpr_desc = desc; texpr_type = t; texpr_loc = e.pexpr_loc; }

and type_expr_desc env loc = function
  | LSE_const c ->
      TE_const c , type_const c

  | LSE_ident x ->
      let x, typ, _ = Gamma.find loc env x in
      TE_ident x , [typ]

  | LSE_unop (Op_not, e) ->
      let tt = [Tbool] in
      let te = expected_type env e tt in
      TE_unop(Op_not, te) , tt

  | LSE_unop (Op_uminus, e) ->
      let te = type_expr env e in
      begin match te.texpr_type with
      | [Tint] -> TE_unop (Op_uminus, te) , [Tint]
      | [Treal] -> TE_unop (Op_uminus_f, te) , [Treal]
      | typ -> error e.pexpr_loc (ExpectedNum (typ))
      end

  | LSE_unop (Op_uminus_f, e) ->
      let tt = [Treal] in
      let te = expected_type env e tt in
      TE_unop (Op_uminus_f, te) , tt

  | LSE_binop ((Op_and | Op_or | Op_impl as op), e1, e2) ->
      let tt = [Tbool] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_binop (op, te1, te2) , tt

  | LSE_binop ((Op_add | Op_sub | Op_mul | Op_div as op), e1, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      begin match te1.texpr_type, te2.texpr_type with
      | [Tint], [Tint] -> TE_binop (op, te1, te2), [Tint]
      | [(Tint | Treal)], [(Tint | Treal)] ->
        TE_binop(
          real_op_of_int_op op,
          real_expr_of_expr te1,
          real_expr_of_expr te2
        ), [Treal]
      | [(Tint | Treal)], typ -> error e2.pexpr_loc (ExpectedNum (typ))
      | typ, _ -> error e1.pexpr_loc (ExpectedNum (typ))
      end

  | LSE_binop (Op_mod, e1, e2) ->
      let tt = [Tint] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_binop(Op_mod, te1, te2) , tt

  | LSE_binop ((Op_div_f | Op_mul_f | Op_sub_f | Op_add_f as op), e1, e2) ->
      let tt = [Treal] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_binop (op, te1, te2), tt

  | LSE_binop (Op_eq | Op_neq as op, e1, e2) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      begin match ty1, ty2 with
        | [t1], [t2] when t1 = t2 -> TE_binop (op, te1, te2), [Tbool]
        | _ -> error loc (Other "invalid operands to equality")
      end

  | LSE_binop (Op_lt | Op_le | Op_gt | Op_ge as op, e1, e2) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      begin match ty1, ty2 with
      | [Tint], [Tint]
      | [Treal], [Treal] -> TE_binop (op, te1, te2), [Tbool]
      | _ -> error loc (Other "invalid operands to comparison")
      end

  | LSE_if (e1, e2, e3) -> assert false

  | LSE_app (f, el) -> begin
    try
    	let (f, (t_in,t_out)) , is_prim = Delta.find f in
    	let tel = type_args env loc t_in el in
    	let aLSP_node = if is_prim then TE_prim(f, tel) else TE_app(f, tel) in
    	aLSP_node ,
    	begin match t_out with
      	| [] -> assert false
      	| _ -> t_out
    	end
    with Not_found -> error loc (UnboundNode f)
  end

  | LSE_arrow (e1, e2) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      if compatible ty1 ty2 then TE_arrow (te1, te2), ty2
      else error te2.texpr_loc (ExpectedType (ty2, ty1))

  | LSE_fby (e1, e2) ->
    let te1 = type_expr env e1 in
    let ty1 = te1.texpr_type in
    let te2 = type_expr env e2 in
    let ty2 = te2.texpr_type in
    if compatible ty1 ty2 then TE_fby (te1, te2), ty2
    else error te2.texpr_loc (ExpectedType (ty2, ty1))

  | LSE_pre e ->
      let te = type_expr env e in
      TE_pre te, te.texpr_type

  | LSE_current e ->
      let te = type_expr env e in
      TE_current te, te.texpr_type

  | LSE_tuple el as n ->
    not_a_nested_tuple n loc;
    let tel = List.map (type_expr env) el in
    TE_tuple tel,
    (List.map (fun e -> base_typ_of_ty e.texpr_loc e.texpr_type) tel)

  | LSE_when(e, cond, clk) ->
    let idclk = type_when loc env cond clk in
    let te = type_expr env e in
    let ty = te.texpr_type in
    TE_when (te, cond, idclk), ty

  | LSE_merge(e, mat) -> begin
    let tout = List.map (fun (c, e) -> type_expr env e) mat in
    let tin = List.map (fun (c, e) -> type_const c) mat in
    let tyo = all_same loc (List.map (fun x -> x.texpr_type) tout) in
    let tmat = List.combine (List.map fst mat) tout in
    let als = all_same loc tin in
    let te = type_expr env e in
    let tyi = als in
    if compatible te.texpr_type tyi then
      TE_merge(te, tmat), tyo
    else error loc (ExpectedType (tyi, als))
  end

and type_when loc env cond clk =
  let tycond = type_const cond in
  let idclk, tyclk, _ = Gamma.find loc env clk in
  if compatible tycond [tyclk] then idclk
  else error loc (ExpectedType (tycond, [tyclk]))


and type_args env loc params_ty el =
  let tel = List.map (type_expr env) el in
  let actual_types =
    List.rev
      begin
	List.fold_left
	  (fun res te -> List.rev_append te.texpr_type res)
	  [] tel
      end
  in
  let well_typed =
    compatible actual_types params_ty
  in
  if well_typed then tel
  else error loc (ExpectedType (actual_types, params_ty));

and expected_type env e tt =
  let te = type_expr env e in
  let typ = te.texpr_type in
  if typ = tt then te
  else error e.pexpr_loc (ExpectedType (typ, tt))

and expected_base_typpe env e =
  let te = type_expr env e in
  match te.texpr_type with
  | [_] -> te
  |  _ ->  error e.pexpr_loc (ExpectedBase (te.texpr_type))

let rec type_patt env p =
  let desc, t = type_patt_desc env p.ppatt_loc p.ppatt_desc in
  { tpatt_desc = desc; tpatt_type = t; tpatt_loc = p.ppatt_loc; }

and type_patt_desc env loc patt =
  let pl_tyl = List.map
	  (fun x ->
	     match Gamma.find loc env x with
	     | x, typ, Vpatt -> x, typ
	     | _  -> error loc (InputVar x)
   )
   patt
   in
   List.split pl_tyl


let type_equation env eq =
  let patt = type_patt env eq.lseq_patt in
  let expr = type_expr env eq.lseq_expr in
  let well_typed = compatible expr.texpr_type patt.tpatt_type in
  if well_typed then
    { teq_patt = patt; teq_expr = expr; }
  else
    error
      eq.lseq_expr.pexpr_loc (ExpectedType (expr.texpr_type, patt.tpatt_type))


let add_vars_of_patt loc s {teq_patt = {tpatt_desc=p}} =
  let add x s =
    if S.mem x s then error loc (Clash x.Ident.name);
    S.add x s
  in
  List.fold_left (fun s x -> add x s) s p

let check_outputs loc env equs =
  let s = List.fold_left (add_vars_of_patt loc) S.empty equs in
  let not_defined = S.diff (Gamma.patts_vars env) s in
  if not (S.is_empty not_defined) then
    error loc (UndefinedOutputs
                 (List.map (fun x -> x.Ident.name) (S.elements not_defined)))

let type_node n =
  let env = Gamma.adds n.lsn_loc Vpatt Gamma.empty (n.lsn_outputs@n.lsn_locals) in
  let env = Gamma.adds n.lsn_loc Vinput env n.lsn_inputs in
  let equs = List.map (type_equation env) n.lsn_eqs in
  check_outputs n.lsn_loc env equs;
  let t_in = List.map (fun (_, typ, clk) -> typ) n.lsn_inputs in
  let t_out = List.map (fun (_, typ, clk) -> typ) n.lsn_outputs in
  let name = Delta.add n.lsn_name (t_in,t_out) in
  let type_decl = fun (x, typ, clk ) ->
      let x', _, _ = Gamma.find n.lsn_loc env x in
      let tclk = match clk with
        | PBase -> TBase
        | PClk(clk, cond) -> begin
            let teclk = type_when n.lsn_loc env cond clk in
            TClk(teclk, cond)
          end in
      (x', typ, tclk) in
  let input =
    List.map type_decl n.lsn_inputs
  in
  let output =
    List.map type_decl n.lsn_outputs
  in
  let local =
    List.map type_decl n.lsn_locals
  in
  let node =
    { tn_name = name;
      tn_input = input;
      tn_output = output;
      tn_local = local;
      tn_equs = equs;
      tn_loc = n.lsn_loc; }
  in
  node

let only_nodes ln =
  let rec aux l = function
    | [] -> l
    | h::t -> begin match h with
        | LS_Node n -> aux (n::l) t
        | LS_Constant _ -> aux l t
      end
  in
  List.rev (aux [] ln)

let type_program f = List.map type_node (only_nodes f)

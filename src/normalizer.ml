open Lustre_typed_ast

let new_local =
  let cpt = ref 0 in
  fun () -> incr cpt; Ident.make ("aux'"^(string_of_int !cpt)) Ident.Stream

let new_pat ({ texpr_type= ty; texpr_loc = loc } as e) =
  match ty with
  | [t] ->
    let x = new_local() in
    let decl = [x,t] in
    let patt = { tpatt_desc = [x]; tpatt_type = ty; tpatt_loc = loc } in
    let expr = { e with texpr_desc = TE_ident x } in
    decl, patt, expr
  | lt ->
    let lx = List.map (fun _ -> new_local()) lt in
    let decl = List.combine lx lt in
    let patt = { tpatt_desc = lx; tpatt_type = ty; tpatt_loc = loc } in
    let le = List.map
      (fun (x,t) ->
        { texpr_desc = TE_ident x; texpr_type = [t]; texpr_loc = loc}
      ) decl
    in
    decl, patt, { e with texpr_desc = TE_tuple le }

let rec normalize ctx e =
  match e.texpr_desc with
  | TE_const _ | TE_ident _ -> ctx, e

  | TE_unop(op,e1) ->
    let ctx, e1' = normalize ctx e1 in
    ctx, { e with texpr_desc = TE_unop(op,e1') }

  | TE_binop(op,e1,e2) ->
    let ctx, e1' = normalize ctx e1 in
    let ctx, e2' = normalize ctx e2 in
    ctx, {e with texpr_desc = TE_binop(op, e1', e2')}

  | TE_app(n,le) ->
    let (new_vars,new_eqs), le' = normalize_list ctx le in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { teq_patt = x_patt;
        teq_expr = { e with texpr_desc = TE_app(n,le') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_prim(n,le) ->
    let (new_vars,new_eqs), le' = normalize_list ctx le in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { teq_patt = x_patt;
        teq_expr = { e with texpr_desc = TE_prim(n,le') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_if(e1,e2,e3) ->
    let ctx, e1' = normalize ctx e1 in
    let ctx, e2' = normalize ctx e2 in
    let ctx, e3' = normalize ctx e3 in
    ctx, { e with texpr_desc = TE_if(e1',e2',e3') }

  | TE_tuple l ->
    let ctx, l' = normalize_list ctx l in
    ctx, { e with texpr_desc = TE_tuple l'}

  | TE_pre e1 ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { teq_patt = x_patt;
        teq_expr = { e with texpr_desc = TE_pre e1' }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_current e1 ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { teq_patt = x_patt;
        teq_expr = { e with texpr_desc = TE_current e1' }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_when (e, cond, clk) ->
    let ctx, e' = normalize ctx e in
    let ctx, clk' = normalize ctx clk in
    ctx, {e with texpr_desc = TE_when(e', cond, clk')}

  | TE_merge (id, le) -> assert false

  | TE_arrow (c,e1) ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { teq_patt = x_patt;
        teq_expr = { e with texpr_desc = TE_arrow(c, e1') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

  | TE_fby(c,e1) ->
    let (new_vars,new_eqs), e1' = normalize ctx e1 in
    let x_decl, x_patt, x_expr = new_pat e in
    let x_eq =
      { teq_patt = x_patt;
        teq_expr = { e with texpr_desc = TE_fby(c, e1') }; }
    in
    (x_decl@new_vars, x_eq::new_eqs), x_expr

and normalize_list ctx l =
  let ctx, l =
    List.fold_left
      (fun (ctx,l) e ->
	let ctx, e' = normalize ctx e in
	ctx, e'::l ) (ctx,[]) l
  in ctx, List.rev l

let normalize_equation node e =
  let (locals, new_eqs), e' = normalize ([],[]) e.teq_expr in
  { node with
    tn_local = locals@node.tn_local;
    tn_equs = { e with teq_expr = e' } :: (List.rev new_eqs) @ node.tn_equs }

let normalize_file =
  List.map
    (fun n ->
      let n =
	List.fold_left normalize_equation { n with tn_equs=[] } n.tn_equs
      in
      { n with tn_equs = List.rev n.tn_equs })

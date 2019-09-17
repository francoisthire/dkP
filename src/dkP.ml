module B  = Basic
module E  = Env.Make(Reduction.Default)
module Errors = Errors.Make(E)
module F  = Files
module TC = Processor.TypeChecker(E)
module P  = Parser.Parse_channel
module Pp = E.Printer

let md = B.mk_mident "dkP"

let dkp_eq   = Term.mk_Const B.dloc (B.mk_name md (B.mk_ident "eq"))

let dk_ind   = Term.mk_Const B.dloc (B.mk_name md (B.mk_ident "eq_ind"))

let dk_ind_r = Term.mk_Const B.dloc (B.mk_name md (B.mk_ident "eq_ind_r"))


let rec vars_of_pat =
  let open Rule in
  function
  | Brackets _ -> failwith "Brackets are not handled yet"
  | Var(_,id,_, _) -> [Term.mk_Const B.dloc (B.mk_name (E.get_name ()) id)]
  | Lambda(_,_,p) -> vars_of_pat p
  | Pattern(_,_,ps) -> List.concat (List.map vars_of_pat ps)

let gen_rule_name =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Rule.Gamma(true, B.mk_name md (B.mk_ident ("dkp_rule" ^ (string_of_int !cpt))))

let meta_rule reverse rule typ predicate eq : Rule.untyped_rule =
  let open Rule in
  let vars = vars_of_pat rule.pat in
  let ind = if reverse then dk_ind_r else dk_ind in
  let rhs = Term.mk_App2 ind [typ; predicate; Term.mk_App2 eq vars] in
  {name=gen_rule_name (); ctx=rule.ctx ;pat=rule.pat;rhs=rhs}

let test reverse ctx hole_name pseudo_predicate term rule eq =
  let ty = E.infer ~ctx term in
  let predicate = Term.mk_Lam B.dloc hole_name (Some ty) pseudo_predicate in
  meta_rule reverse rule ty predicate eq

(* Not efficient at all because beta redex are created *)
let rec find_predicate left right =
  if Term.term_eq left right then fun _ -> left
  else
  match left,right with
  | Term.Lam(_,_, None,te),Term.Lam(lc,x, None, te') when Term.term_eq te te'->
    fun hole -> Term.mk_Lam lc x  None ((find_predicate te te') hole)
  | Term.Lam(_,_, Some ty,te),Term.Lam(lc,x, Some ty', te') when Term.term_eq te te' ->
    fun hole -> Term.mk_Lam lc x (Some ((find_predicate ty ty') hole)) te'
  | Term.Lam(_,_, _,te),Term.Lam(lc,x, Some ty, te')  ->
    fun hole -> Term.mk_Lam lc x (Some ty) ((find_predicate te te') hole)
  | Term.Pi(_,_,tya,tyb), Term.Pi(lc,x,tya',tyb') when Term.term_eq tyb tyb' ->
    fun hole -> Term.mk_Pi lc x ((find_predicate tya tya') hole) tyb'
  | Term.Pi(_,_,_,tyb), Term.Pi(lc,x,tya',tyb') ->
    fun hole -> Term.mk_Pi lc x tya' ((find_predicate tyb tyb') hole)
  | Term.App(_,a,args), Term.App(_,a',args') when List.for_all2 (fun a a' -> Term.term_eq a a') (a::args) (a'::args') -> fun x -> Term.mk_App2 x (a'::args')
  | Term.App _, Term.App _ -> failwith "todo"
  | _ -> assert false

let fresh_name =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    B.mk_name md (B.mk_ident ("?DKP?" ^ (string_of_int !cpt)))

let test2 left right term rule eq =
  let side, pseudo_predicate = find_predicate left right in
  let name = Term.mk_Const B.dloc (fresh_name ()) in
  test side _ name (pseudo_predicate name) term rule eq

let decl_of_rw : Rule.typed_rule -> Entry.entry = fun _ -> failwith "todo"

let cmd_options =
  [ ( "-o"
    , Arg.String (fun s -> F.mk_dir (F.output_directory) s; Basic.add_path s)
    , " (MANDATORY) Set the output directory" )
  ; ( "-l"
    , Arg.Unit (fun () -> ())
    , " Debug flag" )
  ; ( "-I"
    , Arg.String Basic.add_path
    , " DIR Add the directory DIR to the load path" )
  ]

let handle_entry : B.mident -> Entry.entry -> Entry.entry = fun _ _ -> failwith "todo"

let run_on_file in_path =
  let in_file = F.in_from_string in_path `Input in
  let _ = E.init in_path in
  let entries = P.parse in_file.md (F.in_channel_of_file in_file) in
  let entries' = List.map (handle_entry in_file.md) entries in
  let out_file = F.out_from_string in_path `Output in
  let out_fmt = F.fmt_of_file out_file in
  List.iter (Pp.print_entry out_fmt) entries';
  F.close in_file;
  F.close out_file

let _ =
  try
    let options = Arg.align cmd_options in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
    let usage = usage ^ "Available options:" in
    let files =
      let files = ref [] in
      Arg.parse options (fun f -> files := f :: !files) usage;
      List.rev !files
    in
    List.iter run_on_file files
  with
  | Env.EnvError(md,l,e) -> Errors.fail_env_error(md,l,e)
  | Signature.SignatureError e ->
     Errors.fail_env_error(None,Basic.dloc, Env.EnvErrorSignature e)

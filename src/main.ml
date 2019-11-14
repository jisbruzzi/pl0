type token = Token.t

type options=(string list*bool*bool*bool*bool*bool*bool*bool*bool*bool*bool*bool*bool*bool)(* files,--print-tokens, --print-syntax,--log *)
let get_options ():options=
  let files = ref [] in
  let log_desyntax = ref false in
  let print_tokens_opt = ref false in
  let print_syntax_opt = ref false in
  let print_syntax_result_opt = ref false in
  let log_opt = ref false in
  let log_no_context=ref false in
  let print_interpretation = ref false in
  let log_no_flow = ref false in
  let opt_proc_train = ref false in
  let opt_proc_call = ref false in
  let log_labeled_instructions= ref false in
  let log_unlabeled_instructions= ref false in
  let opt_push_pop=ref false in
  let gather_files = (fun (fname)->files:= (fname :: !files)) in
  begin
    Arg.parse 
    [
      ("--print-tokens",Set(print_tokens_opt),"Activate printing tokens");
      ("--log-syntax-analysis",Set(print_syntax_opt),"Log syntax analysis");
      ("--log",Set(log_opt),"Activate printing tokens");
      ("--log-syntax-result",Set(print_syntax_result_opt),"Activate printing tokens with syntactic labels");
      ("--log-desyntax",Set(log_desyntax),"Activate printing desyntaxization");
      ("--log-interpretation",Set(print_interpretation),"Activate printing tokens with syntactic labels");
      ("--log-no-context",Set(log_no_context),"Log actions without context");
      ("--log-no-flow",Set(log_no_flow),"Log actions without flow");
      ("--opt-proc-train",Set(opt_proc_train),"optimise procedure train skip jumps");
      ("--opt-proc-call",Set(opt_proc_call),"optimise procedure call position");
      ("--opt-push-pop",Set(opt_push_pop),"optimise push pop");
      ("--log-labeled-instructions",Set(log_labeled_instructions),"Log labeled instructions");
      ("--log-unlabeled-instructions",Set(log_unlabeled_instructions),"Log unlabeled instructions");
    ] 
    gather_files 
    "Compilador de PL0 en Ocaml";
    (!files,!print_tokens_opt,!print_syntax_opt,!log_opt,!print_syntax_result_opt,!print_interpretation,!log_desyntax,!log_no_context,!log_no_flow,!opt_proc_train,!opt_proc_call,!opt_push_pop,!log_labeled_instructions,!log_unlabeled_instructions)
  end

let compile (opt:options):unit = 
  try
    match opt with (files,print_tokens,log_syntax,log,log_syntax_result,log_interpretation,log_desyntax,log_no_context,log_no_flow,opt_proc_train,opt_proc_call,opt_push_pop,log_labeled_instructions,log_unlabeled_instructions)->
    match files with
    | hd::tl -> hd 
      |> ReadFile.read_lazy_file 
      |> (if log then ReadFile.log_lines_and_pass else LazylistOps.pass)
      |> ReadFile.add_coordinates
      |> Tokenize.run
      |> Tokenize.check
      |> (if print_tokens then (LazylistOps.print TokenOps.string_of_token_coords "|") else LazylistOps.pass)
      |> (Verifier.run log_syntax)
      |> (if log_syntax_result then (LazylistOps.print TokenWithLabelsOps.string_of_token_with_label "\n") else LazylistOps.pass)
      |> Desyntax.run
      |> (if log_desyntax then LazylistOps.print ContextChangeOps.as_string "\n" else LazylistOps.pass)
      |> Interpreter.run
      |> (if log_interpretation then (LazylistOps.print ActionOps.string_of_action "\n") else LazylistOps.pass)
      |> ContextRemover.run
      |> (if log_no_context then LazylistOps.print ContextRemoverOps.string_of_contextualized "\n" else LazylistOps.pass)
      |> FlowRemover.run
      |> (if opt_proc_train then ProcedureTrainOptimiser.run else LazylistOps.pass)
      |> (if opt_proc_call then ProcedureCallOptimiser.run else LazylistOps.pass)
      |> (if log_no_flow then LazylistOps.print FlowActionOps.to_string "\n" else LazylistOps.pass)
      |> Simplifier.run
      |>(if opt_push_pop then PushPopOptimiser.run else LazylistOps.pass)
      |> (if log_labeled_instructions then LazylistOps.print LabeledInstructionOps.to_string "\n" else LazylistOps.pass)
      |> Unlabeller.run
      |> (if log_unlabeled_instructions then LazylistOps.print UnlabeledInstructionOps.to_string "\n" else LazylistOps.pass)
      |> LazylistOps.run_all
    |[]->()
  with 
  |SyntaxError.SyntaxException (e,tc)->(
    print_string(
      (SyntaxError.string_of_error e)^
      (TokenOps.string_of_token_coords tc)^
      "\n"
    );
    exit 1
  )
  |BadTokenException.BadTokenException(t)->
    (print_string ((TokenOps.string_of_token_coords t)^"\n" ) )
  |BadActionException.BadActionException(a)->
    (print_string ("BAD ACTION: "^(ActionOps.string_of_action a)^"\n"))
  
let () =
  get_options () |> compile
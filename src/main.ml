type token = Token.t

type options=(string list*bool*bool*bool*bool*bool)(* files,--print-tokens, --print-syntax,--log *)
let get_options ():options=
  let files = ref [] in
  let print_tokens_opt = ref false in
  let print_syntax_opt = ref false in
  let print_syntax_result_opt = ref false in
  let log_opt = ref false in
  let print_interpretation = ref false in
  let gather_files = (fun (fname)->files:= (fname :: !files)) in
  begin
    Arg.parse 
    [
      ("--print-tokens",Set(print_tokens_opt),"Activate printing tokens");
      ("--log-syntax-analysis",Set(print_syntax_opt),"Log syntax analysis");
      ("--log",Set(log_opt),"Activate printing tokens");
      ("--log-syntax-result",Set(print_syntax_result_opt),"Activate printing tokens with syntactic labels");
      ("--log-interpretation",Set(print_interpretation),"Activate printing tokens with syntactic labels")
    ] 
    gather_files 
    "Compilador de PL0 en Ocaml";
    (!files,!print_tokens_opt,!print_syntax_opt,!log_opt,!print_syntax_result_opt,!print_interpretation)
  end

(*  (LazylistOps.print ) *)

let compile (opt:options):unit = 
  try
    match opt with (files,print_tokens,log_syntax,log,log_syntax_result,log_interpretation)->
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
      |> LazylistOps.print ContextChangeOps.as_string "\n"
      (*
      |> Interpreter.run
      |> (if log_interpretation then (LazylistOps.print ActionOps.string_of_action "\n") else LazylistOps.pass)
      |> SemanticsVerifier.run
      *)
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
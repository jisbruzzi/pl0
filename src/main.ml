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

let pass_char (arg:'a Lazylist.gen_t)=arg
let rec run_all (arg:'a Lazylist.gen_t)=
  match(arg()) with 
  |Empty->()
  |Cons(t,lst)->run_all lst


let compile (opt:options):unit = 
  try
    match opt with (files,print_tokens,log_syntax,log,log_syntax_result,log_interpretation)->
    match files with
    | hd::tl -> hd 
      |> ReadFile.read_lazy_file 
      |> (if log then ReadFile.log_lines_and_pass else pass_char)
      |> ReadFile.add_coordinates
      |> Tokenize.run
      |> Tokenize.check
      |> (if print_tokens then TokenOps.print_tokens_coords else pass_char)
      |> (Verifier.run log_syntax)
      |> (if log_syntax_result then TokenWithLabelsOps.print_lazylist else pass_char)
      |> Interpreter.run
      |> (if log_interpretation then ActionOps.print_lazylist else pass_char)
      |> run_all
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
  
let () =
  get_options () |> compile
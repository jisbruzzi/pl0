type token = Token.t

type options=(string list*bool*bool*bool)(* files,--print-tokens, --print-syntax,--log *)
let get_options ():options=
  let files = ref [] in
  let print_tokens_opt = ref false in
  let print_syntax_opt = ref false in
  let log_opt = ref false in
  let gather_files = (fun (fname)->files:= (fname :: !files)) in
  begin
    Arg.parse 
    [
      ("--print-tokens",Set(print_tokens_opt),"Activate printing tokens");
      ("--log-syntax-analysis",Set(print_syntax_opt),"Log syntax analysis");
      ("--log",Set(log_opt),"Activate printing tokens")
    ] 
    gather_files 
    "Compilador de PL0 en Ocaml";
    (!files,!print_tokens_opt,!print_syntax_opt,!log_opt)
  end

let pass_char (arg:'a Lazylist.gen_t)=arg
let kill (arg:'a Lazylist.gen_t)=()

let compile (opt:options):unit = 
  try
    match opt with (files,print_tokens,log_syntax,log)->
    match files with
    | hd::tl -> hd 
      |> ReadFile.read_lazy_file 
      |> (if log then ReadFile.log_lines_and_pass else pass_char)
      |> ReadFile.add_coordinates
      |> Tokenize.run 
      |> (Verifier.run log_syntax)
      |> (if print_tokens then TokenOps.print_tokens_coords else kill)
    |[]->()
  with SyntaxError.SyntaxException (e,tc)->
  print_string(
    (SyntaxError.string_of_error e)^
    (TokenOps.string_of_token_coords tc)^
    "\n"
  ) 
  
let () =
  get_options () |> compile
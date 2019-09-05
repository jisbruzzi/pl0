type token = Token.t

let () =
  "hola.pl0" |> ReadFile.read_lazy_file |> ReadFile.log_lines_and_pass |> Tokenize.run |> TokenOps.print_tokens
type token = Token.t
  

let () =
  "hola.pl0" |> ReadFile.read_lazy_file |> Tokenize.run |> TokenOps.print_tokens
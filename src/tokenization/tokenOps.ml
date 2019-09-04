type token = Token.t

let string_of_token (t:token)=
  match t with
  | Const -> "CONST"
  | Var -> "VAR"
  | Procedure ->  "PROCEDURE"
  | Ident e -> "IDENT:"^e
  | Igual -> "IGUAL"
  | Integer e->"ENTERO:"^e
  | Point->"PUNTO"
  | EndOfFileToken -> "EOF"
  | Nul -> "NULL"
  | Assignation -> "ASIGNACION"
  | Comma -> "COMA"
  | Semicolon -> "PUNTOYCOMA"
  | Equals->"IGUAL_A"
  | Colon -> "DOS PUNTOS"
  | StringTerminal s->"STRING:"^s
  | Less -> "<"
  | Greater -> ">"
  | LessOrEqual -> "<="
  | GreaterOrEqual -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | OpenParenthesis -> "("
  | ClosedParenthesis ->")"
  | Divide -> "/"
  | Times -> "*"
  | Call -> "CALL"
  | Begin->"BEGIN"
  | End->"END"
  | If->"IF"
  | Then->"THEN"
  | While->"WHILE"
  | Do->"DO"
  | Readln->"READLN"
  | Writeln->"WRITELN"
  | Write->"WRITE"
  | Odd->"ODD"

let print_token t=
  print_string ("|"^(string_of_token t))

let rec print_tokens(tokens:token Lazylist.gen_t)=
  match (tokens()) with
  | Empty -> ()
  | Cons(t,lst) -> (print_token t);(print_tokens lst)

let get_ident_or_keyword (s:string):token=
  match String.uppercase_ascii s with
  | "CONST" -> Const
  | "VAR" -> Var
  | "PROCEDURE" -> Procedure
  | "CALL" -> Call
  | "BEGIN" ->Begin
  | "END"->End
  | "IF"->If
  | "THEN" -> Then
  | "WHILE" -> While
  | "DO" -> Do
  | "READLN" -> Readln
  | "WRITELN" ->Writeln
  | "WRITE" -> Write
  | "ODD" -> Odd
  | s_mayus -> Ident(s_mayus)
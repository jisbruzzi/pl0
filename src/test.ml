open Main
open Token
let rec tokens_equal  
  (file_tokens:Token.t list)
  (tokens:Token.t Lazylist.gen_t)
  :bool =
  match tokens (), file_tokens with
  | Lazylist.Cons(t,lst),hd::tl -> 
    if t=hd 
    then (
      (tokens_equal tl lst)
    )
    else (
      TokenOps.print_token(hd);
      TokenOps.print_token(t);
      false
    )
  | Lazylist.Cons(t,lst),[] -> (TokenOps.print_token(t);false)
  | Lazylist.Empty,hd::tl -> (TokenOps.print_token(hd);false)
  | Lazylist.Empty,[] -> (true)

let rec only_tokens (original:TokenWithCoords.t Lazylist.gen_t):Token.t Lazylist.gen_t=
  fun () -> match original () with
    | Empty->Lazylist.Empty
    | Cons(hd,tl) -> match hd with (coords,token)-> Lazylist.Cons(token,only_tokens tl)

let test_scanner (filename:string) (tokens:Token.t list)=
  if 
  ("tst/BIEN-"^filename^".PL0") |> ReadFile.read_lazy_file |> ReadFile.add_coordinates |> Tokenize.run |> only_tokens|> tokens_equal tokens
  then print_string("Prueba "^filename^" ok\n")
  else print_string("Prueba "^filename^" FALLIDA\n")



let ()=
  test_scanner "00" [
    Var;Ident("X");Comma;Ident("Y");Semicolon;
    Procedure; Ident("INICIAR");Semicolon;
    Const;Ident("Y");Equals;Integer("2");Semicolon;
    Procedure;Ident("ASIGNAR");Semicolon;
    Ident("X");Assignation;Ident("Y");Semicolon;
    Call;Ident("ASIGNAR");Semicolon;
    Begin;
    Write;OpenParenthesis;StringTerminal("NUM=");ClosedParenthesis;Semicolon;Readln;OpenParenthesis;Ident("Y");ClosedParenthesis;Semicolon;
    Call;Ident("INICIAR");Semicolon;
    Writeln;OpenParenthesis;StringTerminal("NUM*2=");Comma;Ident("Y");Times;Ident("X");ClosedParenthesis;
    End;Point;
    EndOfFileToken
  ];
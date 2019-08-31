type 'a lazylist = 
  | Cons of 'a * (unit -> 'a lazylist)
  | Empty
type 'a gen_lazylist = unit-> 'a lazylist

let read_lazy_file (name:string) : char gen_lazylist = 
  let file = open_in name in
  let stream = Stream.of_channel file in
  let rec generator=
    fun () -> try
      Cons( (Stream.next stream),generator)
    with Stream.Failure -> (close_in file;Empty)
  in generator

type token=Const|Var|Procedure|Ident of string|Igual|Integer of string |Point|EndOfFileToken|Nul|Assignation|Comma|Semicolon|Equals|Colon|StringTerminal of string|LessOrEqual|Less|GreaterOrEqual|Greater|Plus|Minus|Divide|Times|OpenParenthesis|ClosedParenthesis|Call|Begin|End|If|Then|While|Do|Readln|Writeln|Write|Odd

let get_ident_or_keyword s=
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



type automata_state=
  |Initial
  |TransitionIdentOrKw of string
  |TransitionNumber of string
  |Terminal of (token*char)
  |TerminalNoChar of token
  |EndOfFileState
  |TransitionString of string
  |TransitionOperator of string
  

let string_of_char (c:char):string=String.make 1 c
let string_with_char s c=s^string_of_char c
let character_matches (r:string)(c:char):bool=
  let rr=Str.regexp r in
  let s=string_of_char c in
  Str.string_match rr s 0

type char_type=Decimal|Alphabetic|Other|Space
let get_type (c:char):char_type=
  if character_matches "[0-9]" c then Decimal else
  if character_matches "[A-Za-z]" c then Alphabetic else
  if character_matches "[ \n]" c then Space else
  Other

let next_state (s:automata_state)(c:char option):automata_state=
  match c with
  | None -> EndOfFileState
  | Some(c)->
    (*print_string("%"^(string_of_char c));*)
    match (s,get_type c) with
    |((Initial|Terminal(_,_)|TerminalNoChar(_)),Decimal) -> TransitionNumber( string_of_char c )
    |((Initial|Terminal(_,_)|TerminalNoChar(_)),Alphabetic) -> TransitionIdentOrKw(string_of_char c)
    |((Initial|Terminal(_,_)|TerminalNoChar(_)),Other) -> 
      (
        match c with
        | '.' -> TerminalNoChar(Point)
        | '=' -> TerminalNoChar(Equals)
        | ',' -> TerminalNoChar(Comma)
        | ';' -> TerminalNoChar(Semicolon)
        | '+' -> TerminalNoChar(Plus)
        | '-' -> TerminalNoChar(Minus)
        | '/' -> TerminalNoChar(Divide)
        | '*' -> TerminalNoChar(Times)
        | '(' -> TerminalNoChar(OpenParenthesis)
        | ')' -> TerminalNoChar(ClosedParenthesis)
        | ':' -> TransitionOperator(string_of_char c)
        | '"' -> TransitionString("")
        | _ -> Terminal(Nul,c)
      )
    |((Initial|Terminal(_,_)|TerminalNoChar(_)),Space) -> Initial

    |(TransitionIdentOrKw(s),(Decimal|Alphabetic)) -> TransitionIdentOrKw(string_with_char s c)
    |(TransitionIdentOrKw(s),_) -> Terminal( get_ident_or_keyword(s),c)

    |(TransitionNumber(s),Decimal) -> print_string("D");TransitionNumber(string_with_char s c)
    |(TransitionNumber(s),Space) -> Terminal(Integer(s),c)
    |(TransitionNumber(s),Alphabetic) -> Terminal(Nul,c)
    |(TransitionNumber(s),Other) -> Terminal(Integer(s),c)

    |(TransitionString(s),Other) when c='"' -> TerminalNoChar(StringTerminal(s))
    |(TransitionString(s),_) -> TransitionString(string_with_char s c)

    |(TransitionOperator(":"),Other) when c='=' -> TerminalNoChar(Assignation)
    |(TransitionOperator(":"),_) -> Terminal(Colon,c)
    |(TransitionOperator("<"),Other) when c='=' -> TerminalNoChar(LessOrEqual)
    |(TransitionOperator("<"),_) -> Terminal(Less,c)
    |(TransitionOperator(">"),Other) when c='=' -> TerminalNoChar(GreaterOrEqual)
    |(TransitionOperator(">"),_) -> TerminalNoChar(Greater)
    |(TransitionOperator(_),_) -> Terminal(Nul,c)

    |(EndOfFileState,_)->Terminal(Nul,c)


let rec tokenize (s:automata_state)(file:char gen_lazylist):token gen_lazylist =
  fun ()-> 
  let rec create_next =  fun  (c:char)(f:char gen_lazylist) -> (tokenize ( next_state s (Some c) ) f) 
  in
    match s with
    | Terminal(t,c) -> Cons(
          t,
          create_next c file
        )
    | _ ->
      match (file()) with
      | Empty -> Cons(EndOfFileToken,fun ()->Empty)
      | Cons(c,next_file)->
        match s with
        | TerminalNoChar(t) -> Cons(
            t,
            create_next c next_file
          )
        | EndOfFileState -> Cons(EndOfFileToken,fun ()->Empty)
        | e-> create_next c next_file ()

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

let rec print_tokens(tokens:token gen_lazylist)=
  match (tokens()) with
  | Empty -> ()
  | Cons(t,lst) -> (print_token t);(print_tokens lst)

let rec print_file(f:string gen_lazylist)=
  match (f())with
  | Empty -> ()
  | Cons(s,lst)->(print_string s);(print_file lst)

let () =
  "hola.pl0" |> read_lazy_file |> tokenize Initial |> print_tokens
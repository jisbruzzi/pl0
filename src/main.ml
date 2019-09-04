open TokenOps

type token = Token.t

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


let rec tokenize (s:automata_state)(file:char Lazylist.gen_t):token Lazylist.gen_t =
  fun ()-> 
  let rec create_next =  fun  (c:char)(f:char Lazylist.gen_t) -> (tokenize ( next_state s (Some c) ) f) 
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



let rec print_file(f:string Lazylist.gen_t)=
  match (f())with
  | Empty -> ()
  | Cons(s,lst)->(print_string s);(print_file lst)

let () =
  "hola.pl0" |> ReadFile.read_lazy_file |> tokenize Initial |> print_tokens
type token = Token.t
type token_coords = TokenWithCoords.t

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
  if character_matches "[ \n\t\r]" c then Space else
  Other

type automata_state=
  |Initial
  |TransitionIdentOrKw of Coords.t*string
  |TransitionNumber of Coords.t*string
  |Terminal of (token_coords*CharWithCoords.t)
  |TerminalNoChar of token_coords
  |EndOfFileState
  |TransitionString of Coords.t*string
  |TransitionOperator of Coords.t*string

let next_state (s:automata_state)(c:CharWithCoords.t option):automata_state=
  match c with
  | None -> EndOfFileState
  | Some(cc)->match cc with (coords,c)->
    match (s,get_type c) with
    | ((Initial|Terminal(_,_)|TerminalNoChar(_)),ct) ->
      (match ct with
        | Decimal -> TransitionNumber(coords,string_of_char c )
        | Alphabetic -> TransitionIdentOrKw(coords,string_of_char c)
        | Other -> (
          match c with
          | '.' -> TerminalNoChar(coords,Point)
          | '=' -> TerminalNoChar(coords,Equals)
          | ',' -> TerminalNoChar(coords,Comma)
          | ';' -> TerminalNoChar(coords,Semicolon)
          | '+' -> TerminalNoChar(coords,Plus)
          | '-' -> TerminalNoChar(coords,Minus)
          | '/' -> TerminalNoChar(coords,Divide)
          | '*' -> TerminalNoChar(coords,Times)
          | '(' -> TerminalNoChar(coords,OpenParenthesis)
          | ')' -> TerminalNoChar(coords,ClosedParenthesis)
          | ':' -> TransitionOperator(coords,string_of_char c)
          | '\'' -> TransitionString(coords,"")
          | '<' -> TransitionOperator(coords,"<")
          | '>' -> TransitionOperator(coords,">")
          | _ -> TerminalNoChar(coords,Nul(c))
        )
        | Space -> Initial
      )

    |(TransitionIdentOrKw(coords,s),(Decimal|Alphabetic)) -> TransitionIdentOrKw(coords,string_with_char s c)
    |(TransitionIdentOrKw(coords,s),_) -> Terminal( (coords,TokenOps.get_ident_or_keyword(s)),cc)

    |(TransitionNumber(coords,s),Decimal) -> TransitionNumber(coords,string_with_char s c)
    |(TransitionNumber(coords,s),(Space|Alphabetic|Other)) -> Terminal((coords,Integer(s)),cc)

    |(TransitionString(coords,s),Other) when c='\'' -> TerminalNoChar(coords,StringTerminal(s))
    |(TransitionString(coords,s),_) -> TransitionString(coords,string_with_char s c)

    |(TransitionOperator(coords,":"),Other) when c='=' -> TerminalNoChar(coords,Assignation)
    |(TransitionOperator(coords,":"),_) -> Terminal((coords,Colon),cc)
    |(TransitionOperator(coords,"<"),Other) when c='=' -> TerminalNoChar(coords,LessOrEqual)
    |(TransitionOperator(coords,"<"),Other) when c='>' -> TerminalNoChar(coords,Distinct)
    |(TransitionOperator(coords,"<"),_) -> Terminal((coords,Less),cc)
    
    |(TransitionOperator(coords,">"),Other) when c='=' -> TerminalNoChar(coords,GreaterOrEqual)
    |(TransitionOperator(coords,">"),_) -> TerminalNoChar(coords,Greater)
    |(TransitionOperator(coords,_),_) -> TerminalNoChar(coords,Nul(c))

    |(EndOfFileState,_)->TerminalNoChar(Coords.Coord(0,0),Nul(c))


let rec run (s:automata_state)(file:CharWithCoords.t Lazylist.gen_t):token_coords Lazylist.gen_t =
  fun ()-> 
  let rec create_next =  fun  (cc:CharWithCoords.t)(f:CharWithCoords.t Lazylist.gen_t) -> (run ( next_state s (Some cc) ) f) 
  in
    match s with
    | Terminal(t,cc) -> Cons(
          t,
          create_next cc file
        )
    | _ ->
      match (file(),s) with
      | (Empty,TerminalNoChar(coords,Point)) -> Cons((coords,Point),fun ()->Empty)
      | (Empty,_) -> Cons((Coords.Coord(0,0),EndOfFileToken),fun ()->Empty)
      | (Cons(cc,next_file),TerminalNoChar(t))->Cons(
          t,
          create_next cc next_file
        )
      | (Cons(cc,next_file),EndOfFileState) -> Cons((Coords.Coord(0,0),EndOfFileToken),fun ()->Empty)
      | (Cons(cc,next_file),_)-> create_next cc next_file ()

let run (file:CharWithCoords.t Lazylist.gen_t):token_coords Lazylist.gen_t =
  run Initial file
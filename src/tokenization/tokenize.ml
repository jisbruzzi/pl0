type token = Token.t
type token_coords = TokenWithCoords.t

let string_of_char (c:char):string=String.make 1 c
let string_with_char s c=s^string_of_char c

let character_matches (r:string)(c:char):bool=
  let rr=Str.regexp r in
  let s=string_of_char c in
  Str.string_match rr s 0

type char_type=Decimal of char|Alphabetic of char|Other of char|Space of char|EndOfFile
let get_type (c:CharWithCoords.echar):char_type=
  match c with
  | Char(c)->(
    if character_matches "[0-9]" c then Decimal(c) else
    if character_matches "[A-Za-z]" c then Alphabetic(c) else
    if character_matches "[ \n\t\r]" c then Space(c) else
    Other(c)
  )
  | EndOfFile->EndOfFile

type automata_state=
  |Initial
  |TransitionIdentOrKw of Coords.t*string
  |TransitionNumber of Coords.t*string
  |Terminal of (token_coords*CharWithCoords.t)
  |TerminalNoChar of token_coords
  |TransitionString of Coords.t*string
  |TransitionOperator of Coords.t*string

let create_integer(s:string):Token.t=
  match int_of_string_opt s with
  | Some(_) -> Integer(s)
  | None -> BadInteger(s)

let next_state (s:automata_state)(cc:CharWithCoords.t):automata_state=
  match cc with (coords,c)->
    match (s,get_type c) with
    | ((Initial|Terminal(_,_)|TerminalNoChar(_)),ct) ->
      (match ct with
        | Decimal(c) -> TransitionNumber(coords,string_of_char c )
        | Alphabetic(c) -> TransitionIdentOrKw(coords,string_of_char c)
        | Other(c) -> (
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
        | Space(c) -> Initial
        |EndOfFile->TerminalNoChar(coords,EndOfFileToken)
      )

    |(TransitionIdentOrKw(coords,s),(Decimal(c)|Alphabetic(c))) -> TransitionIdentOrKw(coords,string_with_char s c)
    |(TransitionIdentOrKw(coords,s),_) -> Terminal( (coords,TokenOps.get_ident_or_keyword(s)),cc)

    |(TransitionNumber(coords,s),Decimal(c)) -> TransitionNumber(coords,string_with_char s c)
    |(TransitionNumber(coords,s),(Space(_)|Alphabetic(_)|Other(_)|EndOfFile)) -> Terminal((coords,create_integer s),cc)

    |(TransitionString(coords,s),Other(c)) when c='\'' -> TerminalNoChar(coords,StringTerminal(s))
    |(TransitionString(coords,s),(Other(c)|Space(c))) when c='\n' -> TerminalNoChar(coords,BadString(Some(c)))
    |(TransitionString(coords,s),EndOfFile) -> TerminalNoChar(coords,BadString(None))
    |(TransitionString(coords,s),(Other(c)|Decimal(c)|Alphabetic(c)|Space(c))) -> TransitionString(coords,string_with_char s c)

    |(TransitionOperator(coords,":"),Other(c)) when c='=' -> TerminalNoChar(coords,Assignation)
    |(TransitionOperator(coords,":"),_) -> Terminal((coords,Colon),cc)
    |(TransitionOperator(coords,"<"),Other(c)) when c='=' -> TerminalNoChar(coords,LessOrEqual)
    |(TransitionOperator(coords,"<"),Other(c)) when c='>' -> TerminalNoChar(coords,Distinct)
    |(TransitionOperator(coords,"<"),_) -> Terminal((coords,Less),cc)
    
    |(TransitionOperator(coords,">"),Other(c)) when c='=' -> TerminalNoChar(coords,GreaterOrEqual)
    |(TransitionOperator(coords,">"),_) -> TerminalNoChar(coords,Greater)
    |(TransitionOperator(coords,_),(Decimal(c)|Alphabetic(c)|Other(c)|Space(c))) -> TerminalNoChar(coords,Nul(c))
    |(TransitionOperator(coords,_),EndOfFile) -> TerminalNoChar(coords,EndOfFileToken)


let rec run (s:automata_state)(file:CharWithCoords.t Lazylist.gen_t):token_coords Lazylist.gen_t =
  fun ()-> 
  let create_next =  fun  (cc:CharWithCoords.t)(f:CharWithCoords.t Lazylist.gen_t) -> (run ( next_state s cc ) f) 
  in
    match s with
    | Terminal(t,cc) -> Cons(
          t,
          create_next cc file
        )
    | _ ->
      match (file(),s) with
      | (Empty,TerminalNoChar(t)) -> Cons(t,fun ()->Empty)
      | (Empty,Terminal(t,cc)) -> Cons(t,fun ()->Empty)
      | (Empty,_) -> Empty
      | (Cons(cc,next_file),TerminalNoChar(t))->Cons(
          t,
          create_next cc next_file
        )
      | (Cons(cc,next_file),_)-> create_next cc next_file ()

let run (file:CharWithCoords.t Lazylist.gen_t):token_coords Lazylist.gen_t =
  run Initial file

let rec check (tokens:token_coords Lazylist.gen_t):token_coords Lazylist.gen_t=
  fun () -> match tokens () with
  | Cons(h,nxt) -> ((
    match h with
      | (_,(Token.BadString(_)|Token.Nul(_)|BadInteger(_))) -> raise (BadTokenException.BadTokenException(h))
      |  _ -> ()
    );Cons(h,(check nxt))
  )
  | Empty -> Empty
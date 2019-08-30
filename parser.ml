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

type token=Const|Var|Procedure|Ident of string|Igual|Entero of int |Punto|EndOfFileToken

type automata_state=
  |Initial
  |TransitionIdent of string
  |TransitionNumber of string
  |Terminal of (token*char)
  |EndOfFileState
  |NUL
  |Point

let get_token(s:string): (string * token option) =
  if finaliza_con_separador s then
    (
      (separador s, match token_que_finaliza_con_separador s with
      | "" ->None
      | "=" ->Some(Igual)
      | "CONST" -> Some(Const)
      | "VAR"->Some(Var)
      | "PROCEDURE"->Some(Procedure)
      | "."->Some(Punto)
      | e when int_of_string_opt e <> None -> Some(Entero(int_of_string e))
      | e when  match_con_regex "[A-Za-z]+" e -> Some(Ident(e))
      | e-> (print_string ("<No sé que es:"^e^">");None))
    )
  else (s,None)

let string_of_char (c:char):string=String.make 1 c
let string_with_char s c=s^string_of_char c
let character_matches (r:string)(c:char):bool=
  let rr=Str.regexp r in
  let s=string_of_char c in
  Str.string_match rr s 0

type char_type=Decimal|Alphabetic|Other of char|Space
let get_type (c:char):char_type=
  if character_matches "[0-9]" c then Decimal else
  if character_matches "[A-Za-z]" c then Alphabetic else
  if character_matches "[ \n]" c then Space else
  Other(c)



let next_state (s:automata_state)(c:char option):automata_state=
  match c with
  | None -> EndOfFileState
  | Some(c)->
    match (s,get_type c) with
    | (Initial,Decimal) -> TransitionNumber( string_of_char c )
    | (Initial,Alphabetic) -> TransitionIdent(string_of_char c)
    | (Initial,Other(oc)) -> if oc='.' then Point else NUL
    | (Initial,Space) -> s
    | (TransitionIdent(s),(Decimal|Alphabetic))-> TransitionIdent(string_with_char s c)
    | (TransitionIdent(s),_) -> Terminal( Ident(s),c)
    | (TransitionNumber(s),Decimal)->TransitionNumber(string_with_char s c)
    | (TransitionNumber(s),_) ->Terminal(Entero(s),c)
    | _ -> Initial (* ME HACE RUIDO DESHACERME DE c ACÁ! *)
      


let rec tokenize (s:automata_state)(file:char gen_lazylist):token gen_lazylist =
  fun ()-> 
    match (file()) with
    | Empty -> (tokenize (next_state s None) file)()
    | Cons(c,next_file)->
      match s with
      | EndOfFileState -> Cons(EndOfFileToken,fun ()->Empty)
      | Terminal(t,c) -> Cons(
        t,
        (tokenize ( next_state Initial (Some c) ) next_file)
      )
      | e-> (tokenize (next_state s (Some c)) next_file)()

let print_token t=
  match t with
  | Const -> print_string "|CONST"
  | Var -> print_string "|VAR"
  | Procedure -> print_string "|PROCEDURE"
  | Ident e -> print_string ("|IDENT:"^e)
  | Igual -> print_string ("|IGUAL")
  | Entero e->print_string("|ENTERO")
  | Punto->print_string("|PUNTO")

let rec print_tokens(tokens:token gen_lazylist)=
  match (tokens()) with
  | Empty -> ()
  | Cons(t,lst) -> (print_token t);(print_tokens lst)

let rec print_file(f:string gen_lazylist)=
  match (f())with
  | Empty -> ()
  | Cons(s,lst)->(print_string s);(print_file lst)

let () =
  "hola.pl0" |> read_lazy_file |> tokenize "" |> print_tokens
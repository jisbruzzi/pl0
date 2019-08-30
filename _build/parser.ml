type 'a lazylist = 
  | Cons of 'a * (unit -> 'a lazylist)
  | Empty
type 'a gen_lazylist = unit-> 'a lazylist

let read_lazy_file (name:string) : string gen_lazylist = 
  let file = open_in name in
  let stream = Stream.of_channel file in
  let rec generator=
    fun () -> try
      Cons( String.make 1 (Stream.next stream),generator)
    with Stream.Failure -> (close_in file;Empty)
  in generator


type token=Const|Var|Procedure|Ident of string|Igual|Entero of int |Punto

let match_con_regex (r_string:string)(s:string):bool=
  Str.string_match (Str.regexp r_string) s 0

let separador s=
  String.make 1 (String.get s (String.length s - 1))

let finaliza_con_separador s =
  if String.length s = 0 then false else
  match separador s with
  |"=" -> true
  |","->true
  |";"->true
  |"."->true
  |" "->true
  | "\n"->true 
  | _ ->false

let token_que_finaliza_con_separador s=
  let l=String.length s in 
  let s = String.sub s 0 (l - 1) in
  let s = String.trim s in
  s(*Str.global_replace (Str.regexp "\n")*)


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

let rec tokenize (maybe_token:string)(file:string gen_lazylist):token gen_lazylist =
  fun ()-> 
    match get_token(maybe_token) with
    | (s,Some(t))->Cons(t,tokenize s file)
    | (s,None) -> 
      match (file ()) with
      | Cons(c,next_file) -> (tokenize (s^c) next_file)()
      | Empty -> 
        match s with
        | "" -> Empty
        | e -> (print_string e;(tokenize s (fun () -> Empty))())(*ACÁ LOOPEA POR SIEMPRE!! Es el mismo llamado!!*)

let print_token t=
  match t with
  | Const -> print_string "|CONST"
  | Var -> print_string "|VAR"
  | Procedure -> print_string "|PROCEDURE"
  | Ident e -> print_string ("|IDENT:"^e)
  | Igual -> print_string ("|IGUAL")
  | Entero e->print_string("|ENTERO")

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
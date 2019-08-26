type 'a lazylist = 
  | Cons of 'a * (unit -> 'a lazylist)
  | Empty
type 'a gen_lazylist = unit-> 'a lazylist

type token = Const | Var | Procedure

let file_to_lazylist (name:string) : string gen_lazylist = 
  let file = open_in name in
  let stream = Stream.of_channel file in
  let rec generator=
    fun () -> try
      Cons( String.make 1 (Stream.next stream),generator)
    with Stream.Failure -> Empty
  in generator

type token=Const|Var|Procedure|Ident of string

let get_token(s:string): token option =
  match s with
  | "CONST" -> Some(Const)
  | "VAR"->Some(Var)
  | "PROCEDURE"->Some(Procedure)
  | e->None

let rec tokenize (maybe_token:string)(file:string gen_lazylist):token gen_lazylist =
  fun ()-> 
    match get_token(maybe_token) with
    | Some(t)->Cons(t,tokenize "" file)
    | None -> 
      match (file ()) with
      | Cons(c,next_file) -> (tokenize (maybe_token^c) next_file)()
      | Empty -> Cons(Ident(maybe_token),fun ()->Empty)

let print_token t=
  match t with
  | Const -> print_string "CONST"
  | e->()
  
let rec print_tokens(tokens:token gen_lazylist)=
  match (tokens()) with
  | Empty -> ()
  | Cons(t,lst) -> (print_token t);(print_tokens lst)
type pattern=Pattern.t

let graph=SyntaxGraph.build
type error_fn=pattern->Token.t->SyntaxError.t

type syntax_match_result=
|Next of pattern
|Error of SyntaxError.t
|Matched
|OptionalUnmatched of SyntaxError.t

let rec match_sequence(lst:pattern list)(syntax_match:pattern->syntax_match_result)=
  match lst with
  | Sequence(l)::tl->match_sequence (List.concat [l;tl]) syntax_match
  | hd::[]->syntax_match hd
  | [] -> Error(SyntaxError.NothingExpected)
  | hd::tl->
    match syntax_match hd with
    | Next(p)->Next(Sequence(p::tl))
    | Matched-> Next(Sequence(tl))
    | Error(e)->Error(e)
    | OptionalUnmatched(e)->match_sequence tl  syntax_match

let match_maybe(p:pattern)(syntax_match:pattern->syntax_match_result)=
  match (syntax_match p) with
  | Error(e)->OptionalUnmatched(e)
  | result->result

let match_asterisk(p:pattern)(syntax_match:pattern->syntax_match_result)=
  match syntax_match p with
  | Error(e)->OptionalUnmatched(e)
  | Next(next_pattern)->Next(Sequence([next_pattern;Asterisk(p)]))
  | e->e

let rec match_or(lst:pattern list)(syntax_match:pattern->syntax_match_result)=
  match lst with
  | hd::[]->syntax_match hd
  | [] ->Error(SyntaxError.NothingExpected)
  | hd::tl-> 
    match syntax_match hd with
    | Next(p)->Next(p)
    | Matched->Matched
    | (Error(e)|OptionalUnmatched(e))->match_or tl syntax_match

let rec syntax_match(p:pattern)(t:Token.t):syntax_match_result=
  let next_match=fun(p)->syntax_match p t in
  match p with
  | In(pattern_generator,_)->next_match (pattern_generator ())
  | Match(match_function,match_error,_)-> if match_function t then Matched else Error(match_error)
  | Maybe(p) -> match_maybe p next_match
  | Asterisk(p) -> match_asterisk p next_match
  | Sequence(lst)->match_sequence lst next_match
  | Or(lst)->match_or lst next_match

let rec run (log:bool)(tokens:TokenWithCoords.t Lazylist.gen_t)(tester:pattern):TokenWithCoords.t Lazylist.gen_t =
  match tokens () with
  |Cons(token_coords,lst)->
    let (_,token)=token_coords in
      (if log then (
        (print_string "-------\n");
        (PatternOps.print_pattern tester);
        (print_string "--WITH TOKEN:--");
        (TokenOps.print_token token);
        (print_string "-------\n")
      ));(
        match syntax_match tester token with
        | Next(next_pattern) -> fun () -> Cons(token_coords,(run log lst next_pattern))
        | Error(error) -> raise (SyntaxError.SyntaxException(error,token_coords))
        | e->(fun()->Empty)
      )
  | Empty -> (fun () -> Lazylist.Empty)


let run (log:bool)(tokens:TokenWithCoords.t Lazylist.gen_t):TokenWithCoords.t Lazylist.gen_t =
  run log tokens (graph ())
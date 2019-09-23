type pattern=Pattern.t

let graph=SyntaxGraph.build

type syntax_match_result=
|Next of pattern*(SyntaxLabel.t list)
|Error of SyntaxError.t
|Matched of (SyntaxLabel.t list)
|OptionalUnmatched of SyntaxError.t

let rec match_sequence_accumulating(lst:pattern list)(syntax_match:pattern->syntax_match_result)(errors:SyntaxError.t list):syntax_match_result=
  match lst with
  | Sequence(l)::tl->match_sequence_accumulating (List.concat [l;tl]) syntax_match errors
  | hd::[]->syntax_match hd
  | [] -> Error(SyntaxError.NothingExpected)
  | hd::tl->
    match syntax_match hd with
    | Next(p,labels)->Next(Sequence(p::tl),labels)
    | Matched(labels)-> Next(Sequence(tl),labels)
    | Error(e)->(
      match errors with 
      |[]->Error(e)
      |lst->Error(SyntaxError.AlternativeErrors(e::errors))
    )
    | OptionalUnmatched(e) -> match_sequence_accumulating tl  syntax_match (e::errors)

let match_sequence(lst:pattern list)(syntax_match:pattern->syntax_match_result)=
  match_sequence_accumulating lst syntax_match []

let match_maybe(p:pattern)(syntax_match:pattern->syntax_match_result)=
  match (syntax_match p) with
  | Error(e)->OptionalUnmatched(e)
  | result->result

let match_asterisk(p:pattern)(syntax_match:pattern->syntax_match_result)=
  match syntax_match p with
  | Error(e)->OptionalUnmatched(e)
  | Next(next_pattern,labels)->Next(Sequence([next_pattern;Asterisk(p)]),labels)
  | e->e

let rec match_or_accumulating(lst:pattern list)(syntax_match:pattern->syntax_match_result)(errors:SyntaxError.t list):syntax_match_result=
  match lst with
  | hd::[]->(
    match syntax_match hd with
    | (Error(e)|OptionalUnmatched(e))->Error(SyntaxError.AlternativeErrors(e::errors))
    | e->e
  )
  | [] ->Error(SyntaxError.NothingExpected)
  | hd::tl-> (
    match syntax_match hd with
    | (Error(e)|OptionalUnmatched(e))->match_or_accumulating tl syntax_match (e::errors)
    | e->e
  )

let match_or(lst:pattern list)(syntax_match:pattern->syntax_match_result):syntax_match_result=
  match_or_accumulating lst syntax_match []

let make_labeled_result(p:pattern)(syntax_match:pattern->syntax_match_result)(lbl:SyntaxLabel.t):syntax_match_result=
  match syntax_match p with
  | Next(p,labels)->Next(Labeled(lbl,p),lbl::labels)
  | Matched(lst)->Matched(lbl::lst)
  | o->o
  

let rec syntax_match(p:pattern)(t:Token.t):syntax_match_result=
  let next_match=fun(p)->syntax_match p t in
  match p with
  | In(pattern_generator,_)->next_match (pattern_generator ())
  | Match(match_function,match_error,_)-> if match_function t then Matched([]) else Error(match_error)
  | Maybe(p) -> match_maybe p next_match
  | Asterisk(p) -> match_asterisk p next_match
  | Sequence(lst)->match_sequence lst next_match
  | Or(lst)->match_or lst next_match
  | Labeled(lbl,p)->make_labeled_result p next_match lbl

let rec run (log:bool)(tokens:TokenWithCoords.t Lazylist.gen_t)(tester:pattern):TokenWithLabels.t Lazylist.gen_t =
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
        | Next(next_pattern,labels) -> fun () -> Cons((labels,token_coords),(run log lst next_pattern))
        | Error(error) -> raise (SyntaxError.SyntaxException(error,token_coords))
        | e->(fun()->Empty)
      )
  | Empty -> (fun () -> Lazylist.Empty)

let run (log:bool)(tokens:TokenWithCoords.t Lazylist.gen_t):TokenWithLabels.t Lazylist.gen_t =
  run log tokens (graph ())
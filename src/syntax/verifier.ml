type pattern=Pattern.t

let graph=SyntaxGraph.build

let rec error (p:pattern)(t:Token.t):SyntaxError.t=
  let ok(p:pattern)(t:Token.t):bool = (error p t) = SyntaxError.NoError in
  let error_with(p:pattern)(t:Token.t)(e:SyntaxError.t):SyntaxError.t = 
  let rec add_alternative_error (e_base:SyntaxError.t)(e_added:SyntaxError.t)= 
    (match e_base with
      | SyntaxError.NoMatch(SyntaxError.NothingExpected) -> SyntaxError.NoMatch(SyntaxError.NothingExpected)
      | SyntaxError.NoError -> SyntaxError.NoError
      | SyntaxError.AlternativeErrors(lst)->SyntaxError.AlternativeErrors(e::lst)
      | SyntaxError.NoMatch(e)->add_alternative_error e e_added
      | err -> SyntaxError.AlternativeErrors([err])
    ) in add_alternative_error (error p t) e
  in

  let ret = (match p with
  | Maybe(p)-> if (ok p t) then SyntaxError.NoError else SyntaxError.NoMatch(error p t)
  | Sequence(p::[]) -> (error p t)
  | Sequence(p::tl) -> (
    match (error p t) with
    | SyntaxError.NoMatch(e) -> (error_with (Sequence tl ) t e)
    | e -> e
  ) 
  | Sequence([]) -> SyntaxError.NoMatch(SyntaxError.NothingExpected)
  | Match(fm,fe,_)-> if (fm t) then SyntaxError.NoError else fe
  | Asterisk(p)-> if (ok p t) then SyntaxError.NoError else SyntaxError.NoMatch(error p t)
  | Or(p::[])->(error p t)
  | Or(p::tl)->if (ok p t) then SyntaxError.NoError else let p_next=Pattern.Or(tl) in (error_with p_next t (error p t))
  | Or([])->SyntaxError.NoError 
  | In(p_fun,_)->(error (p_fun () ) t)
  | Nothing -> SyntaxError.NothingExpected
  | NoMatch -> SyntaxError.NothingExpected)

  in (
    (*
    (print_string "\n =======");
    (print_string "checking:");
    (TokenOps.print_token t);
    (print_string "\n =======");
    (PatternOps.print_pattern p);
    (print_string "\n =======");
    (print_string "error:");
    (print_string (SyntaxError.string_of_error ret));
    (print_string "\n =======");
    *)
    ret
    )


let applies(p:pattern)(t:Token.t):bool= (error p t) = SyntaxError.NoError

let rec next (p:pattern)(t:Token.t):pattern=
  match p with
  | Maybe(p)-> if (applies p t) then (next p t) else Pattern.NoMatch
  | Sequence(p::[]) -> (
      match (next p t) with 
      | NoMatch->  NoMatch
      | Nothing->  Nothing
      | n->Sequence(n::[])
    )
  | Sequence(p::tl) -> (
      match (next p t) with 
      | NoMatch->  (next (Sequence(tl)) t)
      | Nothing->  Sequence(tl)
      | Sequence(h::[]) -> Sequence(h::tl)
      | Sequence(l)->Sequence(List.concat[l;tl])
      | n->Sequence(n::tl)
    )
  | Sequence([]) -> Pattern.Nothing
  | Match(_)->Pattern.Nothing
  | Asterisk(p)->if (applies p t) then Sequence([(next p t);Asterisk(p)]) else Pattern.NoMatch
  | Or(p::tl)-> if (applies p t) then (next p t) else (next (Pattern.Or(tl)) t)
  | Or([])->Pattern.Nothing
  | In(p_fun,_)-> (next (p_fun ()) t)
  | Nothing -> Pattern.Nothing
  | NoMatch -> Pattern.NoMatch
  

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
      ));
      if (( error tester token ) = SyntaxError.NoError) 
      then fun () -> Cons(token_coords,(run log lst (next tester token)))
      else raise (SyntaxError.SyntaxException((error tester token),token_coords))
  | Empty -> (fun () -> Lazylist.Empty)


let run (log:bool)(tokens:TokenWithCoords.t Lazylist.gen_t):TokenWithCoords.t Lazylist.gen_t =
  run log tokens (graph ())
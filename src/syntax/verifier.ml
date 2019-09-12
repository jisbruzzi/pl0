type pattern=Pattern.t

let graph=SyntaxGraph.build

let rec error (p:pattern)(t:Token.t):SyntaxError.t=
  let ok(p:pattern)(t:Token.t):bool = (error p t) = SyntaxError.NoError in
  (match p with
  | Maybe(p)-> if (ok p t) then SyntaxError.NoError else SyntaxError.NoMatch (* No importa si se cumple o no*)
  | Sequence(p::tl) -> let e = (error p t) in if e==SyntaxError.NoMatch then (error (Sequence tl ) t) else e
  | Sequence([]) -> SyntaxError.NoMatch
  | Match(fm,fe,_)-> if (fm t) then SyntaxError.NoError else fe
  | Asterisk(p)-> if (ok p t) then SyntaxError.NoError else SyntaxError.NoMatch
  | Or(p::[])->(error p t)
  | Or(p::tl)->if (ok p t) then SyntaxError.NoError else let p=Pattern.Or(tl) in (error p t)
  | Or([])->SyntaxError.NoError 
  (* NINGÚN NEXT DEBE DEJARME EN ESTE ESTADO!! (creo?) *)
  | In(p_fun,_)->(error (p_fun () ) t)
  | Nothing -> SyntaxError.NothingExpected
  | NoMatch -> SyntaxError.NothingExpected)

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
        (TokenOps.print_token token);
        (print_string "-------\n")
      ));
      if (( error tester token ) = SyntaxError.NoError) 
      then fun () -> Cons(token_coords,(run log lst (next tester token)))
      else raise (SyntaxError.SyntaxException((error tester token),token_coords))
  | Empty -> (fun () ->Lazylist.Empty)


let run (log:bool)(tokens:TokenWithCoords.t Lazylist.gen_t):TokenWithCoords.t Lazylist.gen_t =
  run log tokens (graph ())
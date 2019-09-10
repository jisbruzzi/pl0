type pattern=Pattern.t
module G= SyntaxGraph


let rec error (p:pattern)(t:Token.t):SyntaxError.t=
  let ok(p:pattern)(t:Token.t):bool = (error p t) = SyntaxError.NoError in
  match p with
  | Maybe(p)-> SyntaxError.NoError (* No importa si se cumple o no*)
  | Sequence(p::tl) -> (error p t)
  | Sequence([]) -> SyntaxError.NoError
  | Match(fm,fe)-> if (fm t) then SyntaxError.NoError else (fe t)
  | Asterisk(p)-> SyntaxError.NoError
  | Or(p::[])->(error p t)
  | Or(p::tl)->if (ok p t) then SyntaxError.NoError else (error Or(tl) t)
  | Or([])->SyntaxError.NoError 
  (* NINGÚN NEXT DEBE DEJARME EN ESTE ESTADO!! *)
  | In(p_fun)->(error (p_fun () ) t)

let next (p:pattern)(t:Token.t):pattern=p

let rec run (tokens:TokenWithCoords.t Lazylist.gen_t)(tester:pattern):TokenWithCoords.t Lazylist.gen_t =
  match tokens () with
  |Cons(token_coords,lst)->
    let (_,token)=token_coords in
      if (( error tester token ) = SyntaxError.NoError) 
      then fun () -> Cons(token_coords,(run lst (next tester token)))
      else raise (SyntaxError.SyntaxException (error tester token))
  | Empty -> (fun () ->Lazylist.Empty)


let run (tokens:TokenWithCoords.t Lazylist.gen_t):TokenWithCoords.t Lazylist.gen_t =
  run tokens G.program
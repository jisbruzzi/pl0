let rec run(process:'a->'s->('s*'b option))(source:'a Lazylist.gen_t)(current_state:'s):('b Lazylist.gen_t)=
  fun()->
    match source () with
    | Empty->Empty
    | Cons(hd,tl_gen)->
      match (process hd current_state) with
      |(next_state,Some(real_result))->Cons(real_result,run process tl_gen next_state)
      |(next_state,None)->(run process tl_gen next_state)()

let rec print(conv:'a->string)(separator:string)(src:'a Lazylist.gen_t):('a Lazylist.gen_t)=
  fun ()->
  match src () with
  | Empty->Empty
  | Cons(hd,lst)-> (
    hd |> conv |> (fun s->separator^s) |> print_string; Cons(hd,(print conv separator lst))
  )

let pass(arg:'a Lazylist.gen_t)=arg

let rec run_all (arg:'a Lazylist.gen_t):unit=
  match(arg()) with 
  |Empty->()
  |Cons(t,lst)->run_all lst
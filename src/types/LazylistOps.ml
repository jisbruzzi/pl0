let rec lazylist_with(lst:'a list)(lst_gen:'a Lazylist.gen_t):('a Lazylist.t)=
  match lst with
  | hd::tl->Cons(hd,fun()-> lazylist_with tl lst_gen)
  | []->lst_gen ()

let rec run(process:'a->'s->('s*'b list*'a list))(source:'a Lazylist.gen_t)(current_state:'s):('b Lazylist.gen_t)=
  fun()->
    match source () with
    | Empty->Empty
    | Cons(hd,tl_gen)->
      match (process hd current_state) with
      |(next_state,hd::tl,lst)->lazylist_with (hd::tl) (run process (fun()->lazylist_with lst tl_gen) next_state)
      |(next_state,[],lst)-> (run process (fun()->lazylist_with lst tl_gen) next_state)()

let rec stateless_run(process:'a->'b option)(source:'a Lazylist.gen_t):('b Lazylist.gen_t)=
  run (fun a n->(1,(match process a with None->[]|Some(e)->[e]),[])) source 1

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
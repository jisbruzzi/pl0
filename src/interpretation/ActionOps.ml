let string_of_action(a:Action.t):string=
  match a with
  |DeclareVariable(v)->"DeclareVariable:"^v
  |DeclareConstant(v,i)-> "DeclareConstant:"^v^"="^i
  |DeclareProcedure(v)->"DeclareProcedure:"^v
  |ReadVariable(v)->"ReadVariable:"^v
  |ReadVariableOrConstant(v)->"ReadVariableOrConstant:"^v
  |BeginContext->"BeginContext"
  |EndContext->"EndContext"
  |CallProcedure(name)->"CallProcedure:"^name
  |WriteVariable(name)->"WriteVariable:"^name
  |OkThen->"OkThen"
  |EndIfBlock->"EndIfBlock"
  |EndWhileBlock->"EndWhileBlock"
  |WriteVariableFromInput(s)->"WriteVariableFromInput "^s
  |IntegerRead(s)->"IntegerRead "^s
  |Operate(op)->"Operate "

let rec print_lazylist(a:Action.t Lazylist.gen_t):Action.t Lazylist.gen_t=
  match a () with
  | Empty->a
  | Cons(hd,tl)->(
    (print_string ((string_of_action hd)^"\n") );
    fun () -> Cons(hd,print_lazylist tl)
  )

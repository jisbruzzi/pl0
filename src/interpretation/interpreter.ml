type interpreter_state=
|BeginProgram
|Terminal of ActionWithContext.t

let next_state(s:interpreter_state)(token:TokenWithLabels.t):interpreter_state=
  match s with
  | BeginProgram->Terminal({
    action=Action.ReadVariableOrConstant("nop");
    contexts=[]
  })
  | Terminal(a)->BeginProgram


let rec run(tokens:TokenWithLabels.t Lazylist.gen_t)(state:interpreter_state):ActionWithContext.t Lazylist.gen_t=
  let elem=tokens () in match elem with
  | Empty->(fun ()->Empty)
  | Cons(hd,gen)->
    (fun()->
      match next_state state hd with
      |Terminal(action)->Cons(action,run gen (Terminal action ))
      |other->(run gen other)()
    )

let run(tokens:TokenWithLabels.t Lazylist.gen_t):ActionWithContext.t Lazylist.gen_t=
  run tokens BeginProgram
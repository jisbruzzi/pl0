type remover_ctx=
|LabelAfterReturn of int (* cuando se encuentre un return, se pone este label *)
|LabelAfterEndIf of int
|EndWhile of {jump_before:int;label_after:int}

type remover_state= remover_ctx list

let remove_flow(a:ContextualizedAction.t)(state:remover_state):(remover_state*FlowAction.t list* ContextualizedAction.t list)=
  let state,results = (
    match a with
    |ReadVariable(id)->(state,[ReadVariable(id)])
    |CallProcedure(id)->(state,[CallJump(label_of_procedure state id)])
    |WriteVariable(id)->(state,[WriteVariable(id)])
    |WriteVariableFromInput(id)->(state,[WriteVariableFromInput(id)])
    |IntegerRead(n)->(state,[IntegerRead(n)])
    |Operate(op)->(state,[Operate(op)])
    |PrintNewline->(state,[PrintNewline])
    |PrintString(s)->(state,[PrintString(s)])
    |PrintResult->(state,[PrintResult])
    |ReadVariableFromInput(id)->(state,[ReadVariableFromInput(id)])

    |OkThen->(ok_then state)
    |EndIfBlock->(end_if state)
    |BeginWhileBlock->(begin_while state)
    |EndWhileBlock->(end_while state)
    |Return->(return state)
    |DeclareProcedure(id)->(declare_procedure state id)
  )in (state,results,[])
  

let run(actions:ContextualizedAction.t Lazylist.gen_t):FlowAction.t Lazylist.gen_t=
  LazylistOps.run remove_flow actions []
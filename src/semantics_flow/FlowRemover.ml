type remover_ctx=
|LabelAfterReturn of int (* cuando se encuentre un return, se pone este label *)
|LabelAfterEndIf of int
|EndWhile of {jump_before:int;label_after:int}
|EndRepeat of int

type pl_type={procedure_id:int;label:int}
type remover_state= {ctxs:remover_ctx list;next_label_id:int;procedure_labels:pl_type list}

let get_next_label(state:remover_state)=
  (state.next_label_id,{state with next_label_id=state.next_label_id+1})

let state_with_context(state:remover_state)(context:remover_ctx)=
  {state with ctxs=context::state.ctxs}

let current_context_of(state:remover_state):remover_ctx option=
  match (state.ctxs) with
  | hd::tl->Some(hd)
  | []->None

let state_with_procedure_label(state:remover_state)(label:int)(procedure_id:int)=
  {state with procedure_labels={procedure_id;label}::state.procedure_labels}

let label_of_procedure(state:remover_state)(procedure_id:int)=
  (List.find (fun(pl)->pl.procedure_id=procedure_id) state.procedure_labels).label

let jump_conditionally_to (state:remover_state)(label:int)=
  let label_conditional,state=(get_next_label state) in
  (state,[
    (FlowAction.ConditionalJumpTo label_conditional);
    (FlowAction.JumpTo label);
    (FlowAction.LabeledPosition label_conditional)
  ])

let ok_then (state:remover_state)=
  let label_skip_block,state=(get_next_label state) in
  let state = state_with_context state (LabelAfterEndIf label_skip_block) in
  jump_conditionally_to state label_skip_block

let ok_do (state:remover_state)=
  match (current_context_of state) with
  | Some(EndWhile{jump_before=_;label_after=la})->jump_conditionally_to state la
  | _->(state,[])

let begin_while(state:remover_state)=
  let beginning_label,state=(get_next_label state) in
  let ending_label,state=(get_next_label state) in
  let state=state_with_context state (EndWhile {jump_before=beginning_label;label_after=ending_label}) in
  (state,[
    (FlowAction.LabeledPosition beginning_label)
  ])
let begin_repeat(state:remover_state)=
  let beginning_label,state=(get_next_label state) in
  let state=state_with_context state (EndRepeat(beginning_label) ) in
  (state,[
    (FlowAction.LabeledPosition beginning_label)
  ])


let declare_procedure (state:remover_state) (id:int)=
  let label_skip_all,state=(get_next_label state) in
  let label_this_procedure,state=(get_next_label state) in
  let state = state_with_context state (LabelAfterReturn label_skip_all) in
  let state=state_with_procedure_label state label_this_procedure id in
  (state,[
    (FlowAction.SkipProcedureJumpTo label_skip_all);
    (FlowAction.ProcedureDeclarationLabeledPosition label_this_procedure)
  ])

let state_without_current_context (state:remover_state)=
  {state with ctxs=List.tl state.ctxs}

let unstack_context(state:remover_state)=
  match (current_context_of state) with
  |Some(LabelAfterEndIf(l))->(state_without_current_context state,[FlowAction.LabeledPosition l])
  |Some(LabelAfterReturn(l))->(state_without_current_context state,[FlowAction.Return;FlowAction.SkipProcedureLabeledPosition l])
  |Some(EndWhile{jump_before=jb;label_after=la})->(state_without_current_context state,[
    FlowAction.JumpTo jb;
    FlowAction.LabeledPosition la
  ])
  |Some(EndRepeat(l))->(
      let state=state_without_current_context state in 
      jump_conditionally_to state l
    )

  |None->(state,[])

let remove_flow(a:ContextualizedAction.t)(state:remover_state):(remover_state*FlowAction.t list* ContextualizedAction.t list)=
  let state,(results:FlowAction.t list) = (
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

    |OkThen->(ok_then state)
    |OkDo->(ok_do state)
    |BeginWhileBlock->(begin_while state)
    |BeginRepeatUntilBlock->(begin_repeat state)
    |DeclareProcedure(id)->(declare_procedure state id)

    |EndIfBlock->(unstack_context state)
    |EndWhileBlock->(unstack_context state)
    |Return->(unstack_context state)
    |EndRepeatUntilBlock->(unstack_context state)
  )in (state,results,[])
  

let run(actions:ContextualizedAction.t Lazylist.gen_t):FlowAction.t Lazylist.gen_t=
  LazylistOps.run remove_flow actions {ctxs=[];next_label_id=0;procedure_labels=[]}
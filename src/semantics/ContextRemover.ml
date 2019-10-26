type declaration=
|Variable of (string*int)
|Constant of (string*string)
|Procedure of (string*int)
type context=declaration list
type remover_state={
  contexts:context list;
  next_variable_identifier:int;
  next_procedure_identifier:int;
}
exception CantDeclareVariable of string
exception CantDeclareConstant of string
exception CantDeclareProcedure of string

exception CantFindVariable of string
exception CantFindConstant of string
exception CantFindProcedure of string
exception CantFindVarOrConst of string
let push_context (s:remover_state)={s with contexts=[]::s.contexts}
let pop_context (s:remover_state)={s with contexts=(match s.contexts with hd::tl->tl|[]->[])}

let named (name:string)(d:declaration):bool=
  match d with
  |Variable(n,_)->n=name
  |Constant(n,_)->n=name
  |Procedure(n,_)->n=name

let name_exists_in_current_context(s:remover_state)(name:string)=
  match s.contexts with
  | current::_-> List.exists (named name) current
  | []->false

let raise_if_cant_add_declaration(s:remover_state)(d:declaration):unit=
  match d with
  |Variable(name,id)->
    if name_exists_in_current_context s name then
    raise (CantDeclareVariable name)
  |Constant(name,value)->
    if name_exists_in_current_context s name then
    raise (CantDeclareConstant name)
  |Procedure(name,id)->
    if name_exists_in_current_context s name then
    raise (CantDeclareProcedure name)

let with_declaration(s:remover_state)(d:declaration):remover_state=
  raise_if_cant_add_declaration s d;
  match s.contexts with
  | []->{s with contexts=[[d]]}
  | current_context::tl->{s with contexts=(d::current_context)::tl}

let add_variable (s:remover_state)(name:string):remover_state=
  {
    (with_declaration s (Variable(name,s.next_variable_identifier)))
    with next_variable_identifier=s.next_variable_identifier+1
  }
let add_constant (s:remover_state)(name:string)(value:string):remover_state=
  with_declaration s (Constant(name,value))

let add_procedure(s:remover_state)(name:string):remover_state=
  {
    (with_declaration s (Procedure(name,s.next_procedure_identifier)))
      with next_procedure_identifier=s.next_procedure_identifier+1
  }

let rec named_declaration_ctx (name:string)(c:context):declaration option=
  List.find_opt (named name) c

let rec named_declaration (name:string)(c:context list):declaration option=
  match c with
  | []->None
  | hd::tl->(
    match (named_declaration_ctx name hd) with
    | None->named_declaration name tl
    | Some(d)->Some(d)
  )

let address_of_procedure(s:remover_state)(name:string):int=
  match (named_declaration name s.contexts) with
  | Some(Procedure(_,address))->address
  | _-> raise (CantFindProcedure name)

let address_of_variable(s:remover_state)(name:string):int=
  match (named_declaration name s.contexts) with
  | Some(Variable(_,address))->address
  | _->raise (CantFindVariable name)

let read_variable_or_constant(s:remover_state)(name:string):ContextualizedAction.t=
  match (named_declaration name s.contexts) with
  | Some(Variable(_,address))->(ContextualizedAction.ReadVariable address)
  | Some(Constant(_,value))->(ContextualizedAction.IntegerRead value )
  | _->raise (CantFindVarOrConst name)

let create_new_context_list(action:Action.t)(state:remover_state):remover_state=
  match action with
  | BeginContext-> push_context state
  | EndContext->pop_context state
  | DeclareVariable(name) -> add_variable state name
  | DeclareConstant(name,value)->add_constant state name value
  | DeclareProcedure(name)->add_procedure state name
  | _ -> state
let translated_action(action:Action.t)(state:remover_state):ContextualizedAction.t option=
  match action with
  |DeclareVariable(name)->None
  |DeclareConstant(name,value)->None
  |DeclareProcedure(name)->Some(ContextualizedAction.DeclareProcedure(address_of_procedure state name))
  |ReadVariable(name)->Some(ContextualizedAction.ReadVariable(address_of_variable state name ))
  |CallProcedure(name)->Some(ContextualizedAction.CallProcedure(address_of_procedure state name))
  |ReadVariableOrConstant(name)->Some(read_variable_or_constant state name)
  |WriteVariable(name)-> Some(ContextualizedAction.WriteVariable(address_of_variable state name))
  |BeginContext->None
  |EndContext->Some(Return)
  |OkThen->Some(OkThen)
  |OkDo->Some(OkDo)
  |EndIfBlock->Some(EndIfBlock)
  |WriteVariableFromInput(name)->Some(ContextualizedAction.ReadVariableFromInput(address_of_variable state name))
  |BeginWhileBlock->Some(BeginWhileBlock)
  |EndWhileBlock->Some(EndWhileBlock)
  |IntegerRead(value)->Some(IntegerRead(value))
  |Operate(op)->Some(Operate(op))
  |PrintNewline->Some(PrintNewline)
  |PrintString(value)->Some(PrintString(value))
  |PrintResult->Some(PrintResult)



let remover(a:Action.t)(state:remover_state):(remover_state*ContextualizedAction.t list* Action.t list)=
  let new_context_list=create_new_context_list a state in
  let translation=translated_action a new_context_list in
  match translation with
  | None->(new_context_list,[],[])
  | Some(ta)->(new_context_list,[ta],[])

let run(actions:Action.t Lazylist.gen_t):ContextualizedAction.t Lazylist.gen_t=
  LazylistOps.run remover actions {contexts=[[]];next_variable_identifier=0;next_procedure_identifier=0}
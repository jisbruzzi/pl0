type symbol_context={
  variables:string list;
  constants:string list;
  procedures:string list;
}

let symbol_ok(accesor:symbol_context->string list)(st:symbol_context list)(s:string)=
  st |> List.map accesor |> List.concat |> List.exists (fun (x)->(x=s))

let a_variables=(fun (sc)->sc.variables)
let variable_ok=symbol_ok a_variables
let a_procedures=(fun (sc)->sc.procedures)
let procedure_ok=symbol_ok a_procedures
let a_constants=(fun (sc)->sc.constants)
let constant_ok=symbol_ok a_constants

let symbols_ok(st:symbol_context list)(a:Action.t):bool=
  match a with
  |ReadVariable(s)->variable_ok st s
  |CallProcedure(s)->procedure_ok st s
  |ReadVariableOrConstant(s)->(variable_ok st s) || (constant_ok st s)
  |WriteVariable(s)->(variable_ok st s)
  |WriteVariableFromInput(s)->(variable_ok st s)
  |_->true

let empty_context:symbol_context={variables=[];constants=[];procedures=[]}

let with_var(s:symbol_context list)(v:string)=
  match s with
  | hd::tl->{hd with variables=v::hd.variables}::tl
  | []->{empty_context with variables=v::[]}::[]

let with_constant(s:symbol_context list)(v:string)=
  match s with
  | hd::tl->{hd with constants=v::hd.constants}::tl
  | []->{empty_context with constants=v::[]}::[]

let with_procedure(s:symbol_context list)(v:string)=
  match s with
  | hd::tl->{hd with procedures=v::hd.procedures}::tl
  | []->{empty_context with procedures=v::[]}::[]

let next_state(s:symbol_context list)(a:Action.t):symbol_context list=
  match a with
  |BeginContext->empty_context::s
  |DeclareVariable(v)->with_var s v
  |DeclareConstant(c,_)->with_constant s c
  |DeclareProcedure(p)->with_procedure s p
  |EndContext->(match s with hd::tl->tl | []->[])
  |_->s

let string_of_context(s:symbol_context):string=
  "vars:"^(String.concat ", " s.variables)^
  "procs:"^(String.concat ", " s.procedures)^
  "consts:"^(String.concat ", " s.constants)
let string_of_clist(lst:symbol_context list):string=
  String.concat "; " (List.map string_of_context lst)
let print_state(st:symbol_context list):unit=
  print_string (string_of_clist st)

let rec run(a:Action.t Lazylist.gen_t)(state:symbol_context list):Action.t Lazylist.gen_t=
  
  match a () with
  | Empty -> a
  | Cons(hd,tl)->
  (
    (*print_state state;
    print_string (ActionOps.string_of_action hd);*)
    let s = next_state state hd in
      if symbols_ok s hd
      then fun ()->Cons(hd,run tl s)
      else raise (BadActionException.BadActionException hd)
  )

let run(a:Action.t Lazylist.gen_t):Action.t Lazylist.gen_t=run a []
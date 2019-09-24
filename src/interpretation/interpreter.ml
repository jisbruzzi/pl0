type context_explainer=
|RootContext
|ProcedureContext
|ConstantDeclarationContext of string
|PropositionContext
|ExpressionContext
|TermContext
|AssignationContext of string
|IfContext
|WhileContext

type interpreter_state=
|BeginProgram
|Terminal of Action.t list*context_explainer list
|ContextStack of context_explainer list

let state_with_context (s:interpreter_state)(c:context_explainer):interpreter_state=
  match s with
  |BeginProgram->BeginProgram
  |Terminal(a,clst)->Terminal(a,c::clst)
  |ContextStack(clst)->ContextStack(c::clst)

let state_with_action  (s:interpreter_state)(a:Action.t):interpreter_state=
  match s with
  |BeginProgram->BeginProgram
  |Terminal(alst,clst)->Terminal(List.rev (a:: List.rev alst),clst)
  |ContextStack(clst)->Terminal(a::[],clst)

let string_of_action(a:Action.t)=
  match a with
  |DeclareVariable(v)->"DeclareVariable "^v
  |DeclareConstant(n,v)->"DeclareConstant "^n^":"^v
  |DeclareProcedure(n)->"DeclareProcedure "^n
  |ReadVariable(v)->"ReadVariable "^v
  |CallProcedure(n)->"CallProcedure "^n
  |ReadVariableOrConstant(n)->"ReadVariableOrConstant "^n
  |WriteVariable(n)->"WriteVariable "^n
  |BeginContext->"BeginContext "
  |EndContext->"EndContext "
  |OkThen->"OkThen"
  |EndIfBlock->"EndIfBlock"
  |EndWhileBlock->"EndWhileBlock"
  |WriteVariableFromInput (s)->"WriteVariableFromInput "^s
  

let string_of_context(c:context_explainer)=
  match c with
  |RootContext->"RootContext"
  |ProcedureContext->"ProcedureContext"
  |ConstantDeclarationContext(name)->"ConstantDeclarationContext "^name
  |PropositionContext->"PropositionContext"
  |ExpressionContext->"ExpressionContext"
  |TermContext->"TermContext"
  |AssignationContext(name)->"AssignationContext "^name
  |IfContext->"IfContext"
  |WhileContext->"WhileContext"

let string_of_interpreter_state(s:interpreter_state):string=
  match s with
  |BeginProgram->"BeginProgram"
  |Terminal(aactions,ctxs)->"Terminal:"^(String.concat ", " (List.map string_of_action aactions))^" "^(String.concat ", " (List.map string_of_context ctxs))
  |ContextStack(ctxs)->"ContextStack:"^(String.concat ", " (List.map string_of_context ctxs))

let print_interpreter_state(s:interpreter_state):unit=
  print_string ((string_of_interpreter_state s) ^"\n")

let rec next_state(s:interpreter_state)(token:TokenWithLabels.t):interpreter_state=
  
  let labels,(coords,token)=token in
  let ns cl lb = next_state (ContextStack cl) (lb,(coords,token)) in 
  let ret = match s with
  | BeginProgram->next_state (ContextStack []) (labels,(coords,token))
  | Terminal(_,context_list)->next_state (ContextStack context_list) (labels,(coords,token))
  | ContextStack(context_list)->
    match (context_list,labels,token) with
    (* saltear root *)
    | (RootContext::context_list,ProgramRoot::Block::labels,_)->state_with_context (ns context_list labels) RootContext
    | (context_list,ProgramRoot::Block::labels,_)->state_with_context (ns context_list labels) RootContext
    | (RootContext::context_list,labels,_)->(ns context_list labels)

    (* detectar principio y fin de cuerpo de procedure *)
    | (ProcedureContext::context_list,ProcedureDeclaration::Block::labels,_)->state_with_context (ns context_list labels) ProcedureContext
    | (context_list,ProcedureDeclaration::Block::labels,_)->state_with_action (state_with_context (ns context_list labels) ProcedureContext) BeginContext
    | (ProcedureContext::context_list,labels,_)->state_with_action (ns context_list labels) EndContext


    | (ConstantDeclarationContext(name)::context_list,ConstantNameDeclaration::[],Ident(s))->state_with_context (ns context_list labels) (ConstantDeclarationContext name )
    | (context_list,ConstantNameDeclaration::[],Ident(name))->state_with_context (ns context_list []) (ConstantDeclarationContext name)

    | (ConstantDeclarationContext(name)::context_list,ConstantValue::[],Integer(value))->Terminal([DeclareConstant(name,value)],[])

    (* saltear proposition,expression y term, ya veré qué se hace con esos*)
    | (PropositionContext::context_list,Proposition::labels,_)->state_with_context (ns context_list labels) PropositionContext
    | (context_list,Proposition::labels,_)->state_with_context (ns context_list labels) PropositionContext
    | (PropositionContext::context_list,labels,_)-> (ns context_list labels)

    | (ExpressionContext::context_list,Expression::labels,_)->state_with_context (ns context_list labels) ExpressionContext
    | (context_list,Expression::labels,_)->state_with_context (ns context_list labels) ExpressionContext
    | (ExpressionContext::context_list,labels,_)-> (ns context_list labels)

    | (TermContext::context_list,Term::labels,_)->state_with_context (ns context_list labels) TermContext
    | (context_list,Term::labels,_)->state_with_context (ns context_list labels) TermContext
    | (TermContext::context_list,labels,_)-> (ns context_list labels)
    
    (* contexto de asignacion *)
    | (context_list,AssignationProposition::VariableAssign::[],Ident(name))->state_with_context (ns context_list []) (AssignationContext name)
    | (AssignationContext(name)::context_list,AssignationProposition::labels,_)-> state_with_context (ns context_list labels) (AssignationContext name)
    | (AssignationContext(name)::context_list,labels,_)-> state_with_action (ns context_list labels) (WriteVariable(name))

    (* contexto del if *)
    | (IfContext::context_list,IfProposition::labels,Then)->state_with_action (state_with_context (ns context_list labels) IfContext) OkThen
    | (IfContext::context_list,IfProposition::labels,_)->state_with_context (ns context_list labels) IfContext
    | (IfContext::context_list,labels,_)->state_with_action (ns context_list labels) EndIfBlock
    | (context_list,IfProposition::labels,_)->state_with_context (ns context_list labels) IfContext


    (* contexto del while *)
    | (WhileContext::context_list,WhileProposition::labels,Do)->state_with_action (state_with_context (ns context_list labels) WhileContext) OkThen
    | (WhileContext::context_list,WhileProposition::labels,_)->state_with_context (ns context_list labels) WhileContext
    | (WhileContext::context_list,labels,_)->state_with_action (ns context_list labels) EndWhileBlock
    | (context_list,WhileProposition::labels,_)->state_with_context (ns context_list labels) WhileContext

    

    (* insertar acciones de declaración y uso de variables,constantes, procedures  *)
    | (_,VariableDeclaration::[],Ident(s))->Terminal([DeclareVariable(s)],[])
    | (_,ProcedureDeclaration::ProcedureNameDeclaration::[],Ident(s))->Terminal([DeclareProcedure(s)],[])
    | (_,ProcedureNameCall::[],Ident(s))->Terminal([CallProcedure(s)],[])
    | (_,ConstOrVarRead::[],Ident(s))->Terminal([ReadVariableOrConstant(s)],[])
    | (_,VariableAssignFromReadln::[],Ident(s))->Terminal([WriteVariableFromInput(s)],[])

    (* catch-all *)
    | (contexts,labels,token)->ContextStack(contexts)
    in (
      (*
      print_string "==\n";
      print_string "estado inicial:\n";
      print_string "interpreter state inicial: ";
      print_interpreter_state s;
      print_string "token inicial: ";
      TokenWithLabelsOps.print_token_with_label (labels,(coords,token));
      print_string "estado final:  ";
      print_interpreter_state ret;
      *)
      ret
    )

    
let rec make_lazylist_from(actions:Action.t list)(gen:Action.t Lazylist.gen_t):Action.t Lazylist.t=
  match actions with
  | hd::[]->Lazylist.Cons(hd,gen)
  | hd::tl->Lazylist.Cons(hd,fun()->(make_lazylist_from tl gen))
  | []->gen ()


let rec run_interpretation(tokens:TokenWithLabels.t Lazylist.gen_t)(state:interpreter_state):Action.t Lazylist.gen_t=
  (*print_string "---------------\n";*)
  let elem=tokens () in match elem with
  | Empty->(fun ()->Empty)
  | Cons(hd,gen)->
      match next_state state hd with
      |Terminal(actions,lst)->
        fun ()-> make_lazylist_from actions (run_interpretation gen (Terminal (actions,lst) ))
      |other->(run_interpretation gen other)

let run(tokens:TokenWithLabels.t Lazylist.gen_t):Action.t Lazylist.gen_t=
  run_interpretation tokens BeginProgram
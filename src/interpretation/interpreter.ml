type context_explainer=
|ConstantDeclarationContext of string
|AssignationContext of string
|OperateAfterNextContext of Operation.t
|OperableContext of Operation.t

type interpreter_state=
|BeginProgram
|Terminal of Action.t list*context_explainer list
|ContextStack of context_explainer list

let string_of_action=ActionOps.string_of_action
  

let string_of_context(c:context_explainer)=
  match c with
  |ConstantDeclarationContext(name)->"ConstantDeclarationContext "^name
  |AssignationContext(name)->"AssignationContext "^name
  |OperateAfterNextContext(op) -> "OperateAfterNextContext"
  |OperableContext(op)->"OperableContext"

let string_of_interpreter_state(s:interpreter_state):string=
  match s with
  |BeginProgram->"BeginProgram"
  |Terminal(aactions,ctxs)->"Terminal:"^(String.concat ", " (List.map string_of_action aactions))^" "^(String.concat ", " (List.map string_of_context ctxs))
  |ContextStack(ctxs)->"ContextStack:"^(String.concat ", " (List.map string_of_context ctxs))

let print_interpreter_state(s:interpreter_state):unit=
  print_string ((string_of_interpreter_state s) ^"\n")

let rec next_state(state:interpreter_state)(change:ContextChange.t)=
  match state with
  |BeginProgram ->next_state (ContextStack []) change
  |Terminal(_,context_stack) -> next_state (ContextStack context_stack) change
  |ContextStack(context_stack)->
    let oc=fun(op)->ContextStack((OperateAfterNextContext op)::context_stack) in
    match change,context_stack with
    (* contextos de procedimientos *)
    |ContextChange.Arrives(SyntaxLabel.ProcedureBlockDeclaration),_->Terminal([BeginContext],context_stack)
    |ContextChange.Exits(SyntaxLabel.ProcedureBlockDeclaration),_->Terminal([EndContext],context_stack)

    (* declaración de constantes*)
    |ContextChange.Passes(SyntaxLabel.ConstantNameDeclaration,(_,Token.Ident(constant_name))),_->ContextStack(ConstantDeclarationContext(constant_name)::context_stack)
    |ContextChange.Passes(ConstantValue,(_,Token.Integer(value))),ConstantDeclarationContext(name)::[]->Terminal([DeclareConstant(name,value)],context_stack)

    (* Contextos operables*)
    |ContextChange.Arrives(SyntaxLabel.Proposition|SyntaxLabel.Expression|SyntaxLabel.Term|SyntaxLabel.Factor),OperateAfterNextContext(op)::rest_of_stack->ContextStack(OperableContext(op)::rest_of_stack)
    |ContextChange.Arrives(SyntaxLabel.Proposition|SyntaxLabel.Expression|SyntaxLabel.Term|SyntaxLabel.Factor),_->ContextStack(OperableContext(Operation.NoOperation)::context_stack)
    |ContextChange.Exits(SyntaxLabel.Proposition|SyntaxLabel.Expression|SyntaxLabel.Term|SyntaxLabel.Factor),OperableContext(op)::rest_of_stack->Terminal([Operate(op)],rest_of_stack)

    (*prints *)
    |ContextChange.Exits(SyntaxLabel.WriteLineProposition),_->Terminal([PrintNewline],context_stack)
    |ContextChange.Exits(SyntaxLabel.WriteExpression),_->Terminal([PrintResult],context_stack)
    |ContextChange.Passes(WriteExpression,(_,Token.StringTerminal(value))),_->Terminal([PrintString(value)],context_stack)

    (* asignacion *)

    |ContextChange.Passes(SyntaxLabel.VariableAssign,(_,Token.Ident(name))),_->ContextStack(AssignationContext(name)::context_stack)
    |ContextChange.Exits(SyntaxLabel.AssignationProposition),AssignationContext(name)::rest_of_stack->Terminal([WriteVariable(name)],rest_of_stack)

    (* if *)
    |ContextChange.Passes(_,(_,Token.Then)),_->Terminal([OkThen],context_stack)
    |ContextChange.Exits(SyntaxLabel.IfProposition),_->Terminal([EndIfBlock],context_stack)

    (* while *)
    |ContextChange.Passes(_,(_,Token.While)),_->Terminal([BeginWhileBlock],context_stack)
    |ContextChange.Passes(_,(_,Token.Do)),_->Terminal([OkThen],context_stack)
    |ContextChange.Exits(SyntaxLabel.WhileProposition),_->Terminal([EndWhileBlock],context_stack)

    (* insertar acciones de declaración y uso de variables,constantes, procedures *)
    |ContextChange.Passes(label,(coords,token)),_-> (
      match label,token with
      | (VariableDeclaration,Ident(s))->Terminal([DeclareVariable(s)],context_stack)
      | (ProcedureNameDeclaration,Ident(s))->Terminal([DeclareProcedure(s)],context_stack)
      | (ProcedureNameCall,Ident(s))->Terminal([CallProcedure(s)],context_stack)
      | (ConstOrVarRead,Ident(s))->Terminal([ReadVariableOrConstant(s)],context_stack)
      | (VariableAssignFromReadln,Ident(s))->Terminal([WriteVariableFromInput(s)],context_stack)
      | (LiteralInteger,Integer(s))->Terminal([IntegerRead(s)],context_stack)
      | (FactorOperation,Times)-> oc Operation.TimesOperation
      | (FactorOperation,Divide)-> oc Operation.DivideOperation
      | (TermOperation,Plus)-> oc Operation.PlusOperation
      | (TermOperation,Minus)-> oc Operation.MinusOperation
      | (Negation,Minus)-> oc Operation.NegateOperation
      | (Comparator,GreaterOrEqual)-> oc Operation.GreaterOrEqualCheck
      | (Comparator,Greater)-> oc Operation.GreaterCheck
      | (Comparator,LessOrEqual)-> oc Operation.LessOrEqualCheck
      | (Comparator,Less)-> oc Operation.LessCheck
      | (Comparator,Equals)-> oc Operation.EqualsCheck
      | (Comparator,Distinct)-> oc Operation.DistinctCheck
      | (Comparator,Odd)-> oc Operation.OddCheck

      | _->ContextStack(context_stack)
    )
    | _ -> ContextStack(context_stack)
    

let interpret(token:ContextChange.t)(state:interpreter_state):(interpreter_state*Action.t list*ContextChange.t list)=
  match state with
  | Terminal(actions,lst)->(next_state state token, actions,[])
  | _->(next_state state token, [],[])

let run(tokens:ContextChange.t Lazylist.gen_t):Action.t Lazylist.gen_t=
  (*run_interpretation tokens BeginProgram*)
  LazylistOps.run interpret tokens BeginProgram